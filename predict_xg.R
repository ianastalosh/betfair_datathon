
library(corrplot)
library(naniar)
library(mgcv)

# Predicting XG from stats
contains_xg = team_tournament_data %>% filter(!is.na(xg))
no_xg = team_tournament_data %>% filter(is.na(xg))

# Whats missing
vis_miss(contains_xg)

# Correlation matrices
attacking_features = contains_xg %>%
  select(goals, corners, shots, shots_on_target, shots_off_target, possession, xg) %>%
  drop_na()

xg_corr = corrplot(cor(attacking_features),
                   type = 'upper',
                   method = 'number')

# Simple model
predicted_xg_lm = lm(xg ~ is_home + corners + shots + shots_on_target + possession + reds, data = contains_xg)

# GLM with a log link
predicted_xg_log = glm(log(xg) ~ is_home + corners + shots + shots_on_target + possession + reds, data = contains_xg)

predicted_vals_lm = predict(predicted_xg_lm, contains_xg)
predicted_vals_transform = predict(predicted_xg_log, contains_xg)

# Compare
data_with = contains_xg %>%
  mutate(predicted_xg_lm = predicted_vals_lm,
         predicted_xg_transform = predicted_vals_transform)

ggplot(data_with, aes(x = xg, y = predicted_vals_lm)) + geom_point() + geom_abline(intercept = 0, slope = 1, linetype = 'dashed')
ggplot(data_with, aes(x = xg, y = predicted_vals_transform)) + geom_point() + geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

# Predict the XG values for xg and opp xg, for games we don't have xg for
possession_team_features = no_xg %>% select(is_home, corners, shots, shots_on_target, possession, reds)
opp_team_features = no_xg %>% select(is_home, corners = opp_corners, shots = opp_shots, shots_on_target = opp_shots_on_target,
                                     possession = opp_possession, reds = opp_reds)

new_predicted_xg_values = predict(predicted_xg_lm, possession_team_features)
new_predicted_opp_xg_values = predict(predicted_xg_lm, opp_team_features)

# Add xg
games_added_xg = no_xg %>% 
  mutate(derived_xg = new_predicted_xg_values,
         derived_opp_xg = new_predicted_opp_xg_values)


# Join data
team_tournament_data_derived = bind_rows(games_added_xg, contains_xg) %>%
  mutate(derived_xg = ifelse(is.na(derived_xg), xg, derived_xg),
         derived_opp_xg = ifelse(is.na(derived_opp_xg), opp_xg, derived_opp_xg),
         derived_net_xg = derived_xg - derived_opp_xg) %>%
  filter(!is.na(derived_xg))
