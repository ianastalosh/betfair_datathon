
# Elo Model

# Build a simple Elo model based on expected goals
# This will help develop our ranking system

calculate_elo_win_probability = function(rating1, rating2, home_field_advantage = HOME_FIELD_ADVANTAGE) {
  
  tr1 = 10^((rating1 + home_field_advantage)/400)
  tr2 = 10^((rating2)/400)
  
  prob1 = tr1/(tr1 + tr2)
  prob2 = tr2/(tr1 + tr2)
  
  return(list(team1_prob = prob1,
              team2_prob = prob2))
}

update_elo_rating = function(current_elo, prob_winning, result, k_factor, goal_difference, goal_coefficient) {
  
  # The log(1 + goal_difference) thing scales so bigger wins are rewarded more (but with diminishing returns)
  if (result == 1) {
    return(current_elo + k_factor * (1 - prob_winning) * (1 + log(1 + goal_difference)) * goal_coefficient)
  }
  
  if (result == 0) {
    return(current_elo + k_factor * (0 - prob_winning) * (1 + log(1 - goal_difference)) * goal_coefficient)
  }
  
  if (result == 0.33) {
    return(current_elo + k_factor * (0.33 - prob_winning))
  }
  
}

ELO_STARTING_VALUE = 1500
TEST_DATE_START = '2020-01-01'
HOME_FIELD_ADVANTAGE = 30
K_VALUE = 30
REVERSION_FACTOR = 0.5

elo_training_data = team_tournament_data_derived %>%
  select(game_id, tournament, timestamp, date_GMT, name, is_home, goals, derived_xg)

elo_home = elo_training_data %>%
  filter(is_home == 1) %>%
  select(game_id, tournament, timestamp, date_GMT, home_team = name, home_goals = goals, home_xg = derived_xg)

elo_away = elo_training_data %>%
  filter(is_home == 0) %>%
  select(game_id, tournament, timestamp, date_GMT, away_team = name, away_goals = goals, away_xg = derived_xg)

full_elo_training_data = elo_home %>%
  left_join(elo_away, by = c('game_id', 'tournament', 'timestamp', 'date_GMT')) %>%
  mutate(xg_result = case_when(home_xg > away_xg ~ 'home',
                               home_xg < away_xg ~ 'away',
                               home_xg == away_xg ~ 'draw'),
         match_result = case_when(home_goals > away_goals ~ 'home',
                                  home_goals < away_goals ~ 'away',
                                  home_goals == away_goals ~ 'draw'),
         xg_home_diff = home_xg - away_xg,
         xg_away_diff = away_xg - home_xg) %>%
  separate(date_GMT, into = c('ymd', 'time'), sep = ' - ') %>%
  mutate(date_formatted = mdy(ymd),
         season_end = 0)

# Add manually end of season points every 2 years
full_elo_training_data$season_end[509] = 1
full_elo_training_data$season_end[1398] = 1


# Determine which data will be used for training and which will be used for prediction
# training_data = full_elo_training_data %>% filter(date_formatted <= TEST_DATE_START)
# test_data = full_elo_training_data %>% filter(date_formatted > TEST_DATE_START)

training_data = full_elo_training_data


# Run Elo
elo_results = vector(mode = 'list', length = nrow(training_data))

# Initialize elo ratings
all_countries = unique(elo_training_data$name) %>% sort()
initial_ratings = data.frame(team = all_countries, rating = ELO_STARTING_VALUE)

current_ratings = initial_ratings

for (game_index in 1:nrow(training_data)) {
  
  current_game = training_data[game_index, ]
  
  home_team = current_game$home_team
  away_team = current_game$away_team
  
  game_date = current_game$date_formatted
  game_tournament = current_game$tournament
  
  home_team_row = which(current_ratings$team == home_team)
  away_team_row = which(current_ratings$team == away_team)
  
  home_team_rating = current_ratings$rating[home_team_row]
  away_team_rating = current_ratings$rating[away_team_row]
  
  # Determine if neutral game
  home_field_bonus = ifelse(game_tournament %in% INTERNATIONAL_TOURNAMENTS, 0, HOME_FIELD_ADVANTAGE)
  
  # Get probabilities
  win_probabilities = calculate_elo_win_probability(home_team_rating, away_team_rating, home_field_bonus)
  
  result = current_game$xg_result
  actual_result = current_game$match_result
  home_score_difference = current_game$xg_home_diff
  
  # Convert the result to an number
  if (result == 'home') {
    home_result = 1
    away_result = 0
  } else if (result == 'draw') {
    home_result = 0.33
    away_result = 0.33
  } else if (result == 'away') {
    home_result = 0
    away_result = 1
  }
  
  # Turn off the score difference
  home_score_difference = 0
  
  new_home_rating = update_elo_rating(home_team_rating, win_probabilities$team1_prob, home_result,
                                      K_VALUE, home_score_difference, 1)
 
  new_away_rating = update_elo_rating(away_team_rating, win_probabilities$team2_prob, away_result,
                                      K_VALUE, -home_score_difference, 1) 
  
  current_ratings$rating[home_team_row] = new_home_rating
  current_ratings$rating[away_team_row] = new_away_rating
  
  match_data = data.frame(date = game_date,
                          home_team = home_team,
                          home_rating_pre = home_team_rating,
                          away_team = away_team,
                          away_rating_pre = away_team_rating,
                          xg_result = result,
                          match_result = actual_result,
                          home_xg_diff = home_score_difference,
                          new_home_rating = new_home_rating,
                          new_away_rating = new_away_rating)
  
  elo_results[[game_index]] = match_data
  
  if (game_index %% 200 == 0) {
    print(game_index)
  }
  
  # Is reversion date
  end_of_season = current_game$season_end
  
  if (end_of_season == 0) {
    next
  } else {
    current_ratings = current_ratings %>%
      mutate(rating = ifelse(rating >= 1500, 
                             rating - REVERSION_FACTOR * (rating - 1500),
                             rating + REVERSION_FACTOR * (1500 - rating)))
  }
  
}

final_elo_ratings = current_ratings
write.csv(final_elo_ratings, 'data/elo_ratings.csv')

results_history = bind_rows(elo_results)
results_history_home = results_history %>% 
  select(date, team = home_team, opponent = away_team, rating = new_home_rating)
results_history_away = results_history %>%
  select(date, team = away_team, opponent = home_team, rating = new_away_rating)
full_results_history = bind_rows(results_history_home, results_history_away) %>% 
  arrange(team, date)

# Test accuracy
home_team_elo = current_ratings %>% select(home_team = team, home_rating = rating)
away_team_elo = current_ratings %>% select(away_team = team, away_rating = rating)

test_data_with_rating = test_data %>% 
  left_join(home_team_elo, by = 'home_team') %>%
  left_join(away_team_elo, by = 'away_team') %>%
  mutate(tr_a = 10^((home_rating + HOME_FIELD_ADVANTAGE)/400),
         tr_b = 10^((away_rating)/400),
         prob_a = tr_a/(tr_a + tr_b),
         prob_b = tr_b/(tr_a + tr_b),
         xg_result_binary = ifelse(xg_result == 'home', 1, 0),
         log_loss = ifelse(xg_result_binary == 1, -log(prob_a), -log(prob_b)))

# Log Loss
mean(test_data_with_rating$log_loss)

# Get historical draw rates
draws = results_history %>%
  mutate(elo_diff = home_rating_pre - away_rating_pre,
         rating_group = cut(elo_diff, breaks = seq(-1500, 1500, by = 50))) %>%
  tail(n = 2000) %>%
  group_by(rating_group) %>% 
  summarize(games = n(), 
            draw_percentage = mean(match_result == 'draw'))

get_elo_win_probabilities_with_draw = function(elo1, elo2, home_field_advantage = HOME_FIELD_ADVANTAGE) {
  
  # Draw probability determined using https://www.researchgate.net/figure/Probability-distributions-of-win-draw-and-lose-by-Elo-rating-measurement_fig2_309662241
  # with some tweaks, because I couldn't get the formula to work
  
  adj_elo1 = elo1 + home_field_advantage
  adj_elo2 = elo2
  
  rd = adj_elo1 - adj_elo2
  
  p_draw = (sqrt(2*pi*exp(1)))^-1 * exp(-((rd/160)^2)/(2 * exp(2)))
  
  p1 = (1 + 10^(-rd/400))^(-1) - 0.5 * p_draw
  p2 = (1 + 10^(rd/400))^(-1) - 0.5 * p_draw
  
  return(list(team1_win = p1,
              draw = p_draw,
              team2_win = p2))
  
}

# Write the results for each team
team_final_ratings = training_df %>% 
  group_by(team) %>% 
  filter(row_number() == n()) %>%
  select(team, attacking_xg, defensive_xg)

write.csv(team_final_ratings, 'data/team_xg_ratings.csv')
