
library(tidyverse)

tournament_data = read_csv('data/datathon_initial_form_data.csv') %>% 
  select(-c('attendance', 'status', 'referee', 'Game Week')) %>%
  mutate(game_id = paste(home_team_name, away_team_name, timestamp, sep = '-'),
         result = case_when(home_team_goal_count > away_team_goal_count ~ 'home',
                            home_team_goal_count < away_team_goal_count ~ 'away',
                            home_team_goal_count == away_team_goal_count ~ 'draw'))

# Fix data issue with negative xg in excel
tournament_data$home_team_xg[1] = NA
tournament_data$away_team_xg[1] = NA

# Format data into team vs opponent data
home_tournament_data = tournament_data %>% 
  select(game_id, tournament, timestamp, date_GMT, stadium_name,
         name = home_team_name, pre_match_ppg = home_team_pre_match_ppg, ppg = home_ppg,
         goals = home_team_goal_count, half_time_goals = home_team_goal_count_half_time, goal_timings = home_team_goal_timings,
         corners = home_team_corner_count, yellows = home_team_yellow_cards, reds = home_team_red_cards, 
         shots = home_team_shots, shots_on_target = home_team_shots_on_target, shots_off_target = home_team_shots_off_target,
         fouls = home_team_fouls, possession = home_team_possession, xg = home_team_xg,
         opp_name = away_team_name, opp_pre_match_ppg = away_team_pre_match_ppg, opp_ppg = away_ppg,
         opp_goals = away_team_goal_count, opp_half_time_goals = away_team_goal_count_half_time, opp_goal_timings = away_team_goal_timings,
         opp_corners = away_team_corner_count, opp_yellows = away_team_yellow_cards, opp_reds = away_team_red_cards, 
         opp_shots = away_team_shots, opp_shots_on_target = away_team_shots_on_target, opp_shots_off_target = away_team_shots_off_target,
         opp_fouls = away_team_fouls, opp_possession = away_team_possession, opp_xg = away_team_xg) %>%
  mutate(is_home = 1)

away_tournament_data = tournament_data %>% 
  select(game_id, tournament, timestamp, date_GMT, stadium_name,
         name = away_team_name, pre_match_ppg = away_team_pre_match_ppg, ppg = away_ppg,
         goals = away_team_goal_count, half_time_goals = away_team_goal_count_half_time, goal_timings = away_team_goal_timings,
         corners = away_team_corner_count, yellows = away_team_yellow_cards, reds = away_team_red_cards, 
         shots = away_team_shots, shots_on_target = away_team_shots_on_target, shots_off_target = away_team_shots_off_target,
         fouls = away_team_fouls, possession = away_team_possession, xg = away_team_xg,
         opp_name = home_team_name, opp_pre_match_ppg = home_team_pre_match_ppg, opp_ppg = home_ppg,
         opp_goals = home_team_goal_count, opp_half_time_goals = home_team_goal_count_half_time, opp_goal_timings = home_team_goal_timings,
         opp_corners = home_team_corner_count, opp_yellows = home_team_yellow_cards, opp_reds = home_team_red_cards, 
         opp_shots = home_team_shots, opp_shots_on_target = home_team_shots_on_target, opp_shots_off_target = home_team_shots_off_target,
         opp_fouls = home_team_fouls, opp_possession = home_team_possession, opp_xg = home_team_xg) %>%
  mutate(is_home = 0)

team_tournament_data = bind_rows(home_tournament_data, away_tournament_data) %>%
  mutate(goal_difference = goals - opp_goals,
         team_result = case_when(goal_difference > 0 ~ 'win',
                                 goal_difference < 0 ~ 'loss', 
                                 goal_difference == 0 ~ 'draw'),
         log_xg = log(xg),
         net_xg = xg - opp_xg) %>%
  # Remove games where the goal difference was greater than 6 (outliers)
  # Remove games if shots are missing (can't do anything there)
  filter(abs(goal_difference) <= 5,
         !is.na(shots)) 
