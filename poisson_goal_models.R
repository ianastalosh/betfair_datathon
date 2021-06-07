
# Train Poisson models
# Poisson models
poisson_attacking_model = glm(score ~ as.factor(is_home) + 
                                (attacking_xg + defensive_xg + 
                                   opponent_attacking_xg + opponent_defensive_xg)^2,
                              family = 'poisson',
                              data = training_df)

poisson_defensive_model = glm(opponent_score ~ as.factor(is_home) + 
                                (attacking_xg + defensive_xg + 
                                   opponent_attacking_xg + opponent_defensive_xg)^2,
                              family = 'poisson',
                              data = training_df)

get_home_poisson_win_prediction = function(home_attacking_xg, 
                                            home_defensive_xg,
                                            away_attacking_xg,
                                            away_defensive_xg) {
  

  team_variables = data.frame(is_home = 1,
                              attacking_xg = home_attacking_xg,
                              defensive_xg = home_defensive_xg,
                              opponent_attacking_xg = away_attacking_xg,
                              opponent_defensive_xg = away_defensive_xg)
  
  predicted_score_mean = predict(poisson_attacking_model, team_variables, 'response')
  predicted_opponent_mean = predict(poisson_defensive_model, team_variables, 'response')
  
  NUM_GOALS = 0:10
  
  home_goals_probability = data.frame(home_goals = NUM_GOALS) %>% mutate(home_goals_prob = dpois(home_goals, predicted_score_mean))
  away_goals_probability = data.frame(away_goals = NUM_GOALS) %>% mutate(away_goals_prob = dpois(away_goals, predicted_opponent_mean))
  
  score_matrix = expand.grid(home_goals = NUM_GOALS, away_goals = NUM_GOALS) %>%
    left_join(home_goals_probability, by = c('home_goals')) %>%
    left_join(away_goals_probability, by = c('away_goals')) %>%
    mutate(probability = home_goals_prob * away_goals_prob)
  
  event_probabilities = score_matrix %>% 
    mutate(result_type = case_when(home_goals > away_goals ~ 'home_win',
                                   home_goals == away_goals ~ 'draw',
                                   home_goals < away_goals ~ 'away_win')) %>% 
    group_by(result_type) %>%
    summarize(prob = sum(probability))

  # Ensure that probabilities sum to 1
  home_prob = event_probabilities %>% filter(result_type == 'home_win') %>% select(prob) %>% unlist() %>% unname()
  draw_prob = event_probabilities %>% filter(result_type == 'draw') %>% select(prob) %>% unlist() %>% unname()
  away_prob = event_probabilities %>% filter(result_type == 'away_win') %>% select(prob) %>% unlist() %>% unname()
  
  home_scaled = home_prob / sum(event_probabilities$prob)
  draw_scaled = draw_prob / sum(event_probabilities$prob)
  away_scaled = away_prob / sum(event_probabilities$prob)
  
  return(list(home_win = home_scaled,
              draw = draw_scaled,
              away_win = away_scaled))
  
}

get_poisson_win_prediction = function(team1_attacking_xg, 
                                      team1_defensive_xg,
                                      team2_attacking_xg,
                                      team2_defensive_xg,
                                      neutral_game = TRUE) {
  
  team1_home_predictions = get_home_poisson_win_prediction(team1_attacking_xg,
                                                           team1_defensive_xg,
                                                           team2_attacking_xg,
                                                           team2_defensive_xg)
  
  if (neutral_game == FALSE) {
    
    return(list(team1_win = team1_home_predictions$home_win,
                draw = team1_home_predictions$draw,
                team2_win = team1_home_predictions$away_win))
    
  }
  
  team2_home_predictions = get_home_poisson_win_prediction(team2_attacking_xg,
                                                           team2_defensive_xg,
                                                           team1_attacking_xg,
                                                           team1_defensive_xg)
  
  average_team1_win = (team1_home_predictions$home_win + team2_home_predictions$away_win)/2
  average_draw = (team1_home_predictions$draw + team2_home_predictions$draw)/2
  average_team2_win = (team1_home_predictions$away_win + team2_home_predictions$home_win)/2
  
  return(list(team1_win = average_team1_win,
              draw = average_draw,
              team2_win = average_team2_win))
}

get_poisson_win_prediction(1.9, 1.5, 1.2, 1.9)
