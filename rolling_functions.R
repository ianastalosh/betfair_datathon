
library(zoo)
library(lubridate)
library(pracma)
library(plotly)
library(xgboost)

'%!in%' <- function(x,y)!('%in%'(x,y))

INTERNATIONAL_TOURNAMENTS = c('African Cup of Nations', 'CONCACAF Gold Cup', 'Copa America', 'FIFA World Cup', 'FIFA Confederations Cup', 'UEFA Euro')
TODAYS_DATE = Sys.Date()
WINDOW_SIZE = 8

# How many games do we have for each team? 
team_counts = team_tournament_data_derived %>%
  count(name) %>%
  arrange(desc(n))

teams_less_window = team_counts %>% 
  filter(n <= WINDOW_SIZE) %>%
  select(name) %>% 
  unlist() %>% 
  unname()

# Create rolling xg features
team_rolling = team_tournament_data_derived %>%
  filter(name %!in% teams_less_window) %>%
  arrange(name, timestamp) %>%
  separate(date_GMT, into = c('raw_date', 'start_time'), sep = ' - ', remove = FALSE) %>%
  group_by(name) %>%
  mutate(game_number = row_number(),
         formatted_time = mdy(raw_date), 
         offensive_rolling_xg = lag(movavg(derived_xg, n = WINDOW_SIZE, type = 'e')),
         defensive_rolling_xg = lag(movavg(derived_opp_xg, n = WINDOW_SIZE, type = 'e')))

ggplotly(ggplot(team_rolling, aes(x = offensive_rolling_xg, y = defensive_rolling_xg, group = name)) + 
  geom_point(alpha = 0.3))

# Get final ratings for each team
final_ratings = team_rolling %>%
  select(name, date_GMT, offensive_rolling_xg, defensive_rolling_xg) %>%
  group_by(name) %>% 
  filter(row_number() == n()) %>%
  mutate(rating_ratio = offensive_rolling_xg / defensive_rolling_xg)

ggplot(final_ratings, aes(x = offensive_rolling_xg, y = defensive_rolling_xg)) + 
  geom_point(aes(colour = name), alpha = 0.8) + 
  guides(colour = FALSE)

# Create the training data frame
# For each game in the dataset, we need:
# Team A's attacking and defending xg
# Team B's attacking and defending xg

# Create an empty list to fill
match_data = vector(mode = 'list', length = nrow(team_rolling))

for (game_index in 1:nrow(team_rolling)) {
  
  game_row = team_rolling[game_index, ]
  
  current_game_id = game_row$game_id
  game_date = game_row$formatted_time
  team = game_row$name
  opponent = game_row$opp_name
  
  event = game_row$tournament
  
  home_game = game_row$is_home
  if (event %in% INTERNATIONAL_TOURNAMENTS) {
    home_game = 0
  }
  
  attacking_xg = game_row$offensive_rolling_xg
  defensive_xg = game_row$defensive_rolling_xg
  
  # Need to get opponent's offensive and defensive xg
  equivalent_game = team_rolling %>% ungroup() %>% filter(game_id == current_game_id, name != team)
  opp_attacking_xg = equivalent_game$offensive_rolling_xg
  opp_defensive_xg = equivalent_game$defensive_rolling_xg
  
  dodgy = 0
  
  if (length(opp_attacking_xg) == 0) {
    opp_attacking_xg = 1
    dodgy = 1
  }
  
  if (length(opp_defensive_xg) == 0) {
    opp_defensive_xg = 0
    dodgy = 1
  }
  
  # Get result
  score = game_row$goals
  opp_score = game_row$opp_goals
  score_difference = game_row$goal_difference
  result = game_row$team_result
  
  match_data[[game_index]] = data.frame(game_id = current_game_id,
                                        date = game_date,
                                        team = team,
                                        opponent = opponent,
                                        is_home = home_game,
                                        attacking_xg = attacking_xg,
                                        defensive_xg = defensive_xg,
                                        opponent_attacking_xg = opp_attacking_xg,
                                        opponent_defensive_xg = opp_defensive_xg,
                                        score = score,
                                        opponent_score = opp_score,
                                        score_difference = score_difference,
                                        result = result,
                                        untrustworthy = dodgy)
  if (game_index %% 500 == 0) {
    print(game_index)
  }
  
}


training_df = bind_rows(match_data) %>%
  filter(untrustworthy == 0) %>%
  mutate(net_attack = attacking_xg + opp_defensive_xg,
         net_defense = defensive_xg + opp_attacking_xg) %>%
  drop_na()

