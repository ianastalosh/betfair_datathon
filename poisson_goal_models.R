
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
