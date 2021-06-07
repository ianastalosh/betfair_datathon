
# XGBoost Model
xgboost_training_data = training_df %>% 
  select(is_home, attacking_xg, defensive_xg, opponent_attacking_xg, opponent_defensive_xg, result) %>%
  mutate(result_code = case_when(result == 'loss' ~ 0,
                                 result == 'draw' ~ 1,
                                 result == 'win' ~ 2))

xgboost_training_data_x = xgboost_training_data %>% select(-result_code, -result) %>% as.matrix()
xgboost_training_data_y = xgboost_training_data %>% select(result_code) %>% as.matrix()
xgboost_dmatrix = xgb.DMatrix(xgboost_training_data_x, label = xgboost_training_data_y)


testing_grid = expand.grid(max_depth = seq(3, 9, by = 1),
                           eta = seq(0.05, 0.3, by = 0.05),
                           colsample_bytree = seq(0.6, 1, by = 0.1))

testing_results = vector(mode = 'list', length = nrow(testing_grid)) 

# Run sample
for (test_index in 1:nrow(testing_grid)) {
  
  set.seed(2300062)
  current_params = testing_grid[test_index, ]
  
  current_params = list(objective = 'multi:softprob', 
                        max_depth = current_params$max_depth,
                        eta = current_params$eta,
                        colsample_bytree = current_params$colsample_bytree)
  
  current_model = xgb.cv(params = current_params,
                         data = xgboost_dmatrix,
                         nrounds = 2000,
                         nfold = 5,
                         metrics = 'mlogloss',
                         num_class = 3,
                         early_stopping_rounds = 100,
                         stratified = TRUE,
                         prediction = TRUE,
                         verbose = FALSE)
  
  best_iteration = current_model$best_iteration
  best_result = current_model$evaluation_log[best_iteration, ]
  
  testing_results[[test_index]] = data.frame(max_depth = current_params$max_depth,
                                             eta = current_params$eta,
                                             colsample_bytree = current_params$colsample_bytree,
                                             best_iteration = best_iteration,
                                             best_train_result = best_result$train_mlogloss_mean,
                                             best_test_result = best_result$test_mlogloss_mean)
  
  if (test_index %% 20 == 0) {
    print(test_index)
  }
  
}

grid_search_results = bind_rows(testing_results)

params = list(objective = 'multi:softprob', 
              max_depth = 6,
              eta = 0.01,
              gamma = )

xgb_cv_model = xgb.cv(params = params,
                      data = xgboost_dmatrix,
                      nrounds = 5000,
                      nfold = 10,
                      metrics = 'mlogloss',
                      num_class = 3,
                      stratified = TRUE,
                      prediction = TRUE,
                      save_models = TRUE)

xgboost_win_loss_draw_model = xgboost(params = list(max_depth = 3,
                                                    eta = 0.2,
                                                    colsample_bytree = 0.8,
                                                    objective = 'multi:softprob'),
                                      data = xgboost_dmatrix,
                                      nrounds = 30,
                                      num_class = 3,
                                      metrics = 'mlogloss',
                                      verbose = TRUE)

predict_result_xgboost = function(team1_attacking_xg,
                                  team1_defensive_xg,
                                  team2_attacking_xg,
                                  team2_defensive_xg,
                                  neutral_game = TRUE) {
  
  team1_home = matrix(c(1, team1_attacking_xg, team1_defensive_xg, team2_attacking_xg, team2_defensive_xg), 
                      nrow = 1,
                      dimnames = list(NULL, colnames(xgboost_training_data_x)))
  
  team1_home_predictions = predict(xgboost_win_loss_draw_model, team1_home)
  
  if (neutral_game == FALSE) {
    return(list(team1_win = team1_home_predictions[3],
                draw = team1_home_predictions[2],
                team2_win = team1_home_predictions[1]))
  }
  
  team2_home = matrix(c(1, team2_attacking_xg, team2_defensive_xg, team1_attacking_xg, team1_defensive_xg), 
                      nrow = 1,
                      dimnames = list(NULL, colnames(xgboost_training_data_x)))
  team2_home_predictions = predict(xgboost_win_loss_draw_model, team2_home)
  
  
  average_predictions = (team1_home_predictions + rev(team2_home_predictions))/2
  return(list(team1_win = average_predictions[3],
              draw = average_predictions[2],
              team2_win = average_predictions[1]))
}
