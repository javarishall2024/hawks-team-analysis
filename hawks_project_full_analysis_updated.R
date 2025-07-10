# -----------------------------------------------
# Atlanta Hawks Analysis (2021–2024)
# Possession-Adjusted Metrics + Win Model + Visuals
# -----------------------------------------------

# Load libraries
library(hoopR)
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)

# -----------------------------------------------
# STEP 1: Load & Clean Data
# -----------------------------------------------
hawks_data <- lapply(2021:2024, function(season) {
  load_nba_team_box(season = season, team = "Atlanta Hawks")
}) %>% bind_rows() %>% clean_names()

league_data <- load_nba_team_box(2021:2024) %>%
  clean_names() %>%
  filter(team_display_name != "Atlanta Hawks")

# -----------------------------------------------
# STEP 2: Possession-Adjusted Four Factors
# -----------------------------------------------
hawks_factors <- hawks_data %>%
  group_by(season) %>%
  summarise(
    poss = mean(0.5 * (
      (field_goals_attempted + 0.4 * free_throws_attempted -
         1.07 * (offensive_rebounds / (offensive_rebounds + defensive_rebounds)) *
         (field_goals_attempted - field_goals_made) + turnovers) +
        (opponent_team_score + 0.4 * free_throws_attempted -
           1.07 * (offensive_rebounds / (offensive_rebounds + defensive_rebounds)) *
           (field_goals_attempted - field_goals_made) + turnovers)
    ), na.rm = TRUE),
    eFG = mean((field_goals_made + 0.5 * three_point_field_goals_made) / field_goals_attempted * 100, na.rm = TRUE),
    TOV = mean(turnovers / poss * 100, na.rm = TRUE),
    FT_FGA = mean(free_throws_made / field_goals_attempted * 100, na.rm = TRUE)
  )

league_factors <- league_data %>%
  group_by(season) %>%
  summarise(
    eFG_League = mean((field_goals_made + 0.5 * three_point_field_goals_made) / field_goals_attempted * 100, na.rm = TRUE),
    TOV_League = mean(turnovers / (
      0.5 * (
        (field_goals_attempted + 0.4 * free_throws_attempted -
           1.07 * (offensive_rebounds / (offensive_rebounds + defensive_rebounds)) *
           (field_goals_attempted - field_goals_made) + turnovers) +
          (opponent_team_score + 0.4 * free_throws_attempted -
             1.07 * (offensive_rebounds / (offensive_rebounds + defensive_rebounds)) *
             (field_goals_attempted - field_goals_made) + turnovers)
      )
    ) * 100, na.rm = TRUE),
    FT_FGA_League = mean(free_throws_made / field_goals_attempted * 100, na.rm = TRUE)
  )

# -----------------------------------------------
# STEP 3: Merge Data + Net Differentials
# -----------------------------------------------
model_data <- hawks_factors %>%
  left_join(league_factors, by = "season") %>%
  mutate(
    Net_eFG = eFG - eFG_League,
    Net_TOV = TOV_League - TOV,
    Net_FT_FGA = FT_FGA - FT_FGA_League,
    Wins = c(41, 43, 41, 36)  # Real Hawks win totals (2021–2024)
  )

# -----------------------------------------------
# STEP 4: Build Win Prediction Model
# -----------------------------------------------
win_model <- lm(Wins ~ Net_eFG + Net_TOV + Net_FT_FGA, data = model_data)
model_data$Predicted_Wins <- predict(win_model, model_data)

# -----------------------------------------------
# STEP 5: Accuracy Metrics
# -----------------------------------------------
rmse <- sqrt(mean((model_data$Wins - model_data$Predicted_Wins)^2))
mae <- mean(abs(model_data$Wins - model_data$Predicted_Wins))
rsq <- summary(win_model)$r.squared

cat("Model Accuracy Metrics:\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("R-squared:", round(rsq, 4), "\n\n")

# -----------------------------------------------
# STEP 6: Output - Actual vs Predicted
# -----------------------------------------------
print(model_data %>%
        select(season, Wins, Predicted_Wins, Net_eFG, Net_TOV, Net_FT_FGA))

# -----------------------------------------------
# STEP 7: Visualization
# -----------------------------------------------
ggplot(model_data, aes(x = factor(season))) +
  geom_col(aes(y = Wins), fill = "steelblue", width = 0.4, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = Predicted_Wins), fill = "orange", width = 0.4, position = position_nudge(x = 0.2)) +
  labs(title = "Atlanta Hawks: Actual vs Predicted Wins (2021–2024)",
       x = "Season", y = "Wins") +
  theme_minimal(base_size = 14)

