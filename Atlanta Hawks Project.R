# Load the necessary package
library(nbastatR)

# Extract stats for the last five seasons (2020-2024)
nba_data <- bref_teams_stats(seasons = c(2020, 2021, 2022, 2023, 2024))

str(nba_data$dataTable)
nba_data_flat <- as.data.frame(nba_data$dataTable)
colnames(nba_data_flat)
head(nba_data_flat, 10)
names(nba_data_flat)
hawks_data <- nba_data_flat %>%
  filter(nameTeam == "Atlanta Hawks")  # Adjust column name if needed
head(hawks_data)

nba_data_2020 <- bref_teams_stats(seasons = 2020)
nba_data_2021 <- bref_teams_stats(seasons = 2021)
nba_data_2022 <- bref_teams_stats(seasons = 2022)
nba_data_2023 <- bref_teams_stats(seasons = 2023)
nba_data_2024 <- bref_teams_stats(seasons = 2024)

nba_data_2020 <- as.data.frame(nba_data_2020$dataTable)
nba_data_2021 <- as.data.frame(nba_data_2021$dataTable)
nba_data_2022 <- as.data.frame(nba_data_2022$dataTable)
nba_data_2023 <- as.data.frame(nba_data_2023$dataTable)
nba_data_2024 <- as.data.frame(nba_data_2024$dataTable)

nba_data_flat <- bind_rows(nba_data_2020, nba_data_2021, nba_data_2022, nba_data_2023, nba_data_2024)
unique(nba_data_flat$yearSeason)

hawks_data <- nba_data_flat %>% 
  filter(nameTeam == "Atlanta Hawks")

unique(hawks_data$yearSeason)  # Confirm all seasons exist

nba_data_2020 <- as.data.frame(nba_data_2020$dataTable)

dim(nba_data_2020)  # Should return a valid number of rows and columns
head(nba_data_2020) # Preview data

nba_data_flat <- bind_rows(nba_data_2020, nba_data_2021, nba_data_2022, nba_data_2023, nba_data_2024)
unique(nba_data_flat$yearSeason)  # Confirm all seasons exist

hawks_data <- nba_data_flat %>% 
  filter(nameTeam == "Atlanta Hawks")

unique(hawks_data$yearSeason)  # Should now include 2020

# Compute Four-Factor Metrics for the Hawks
hawks_four_factors <- hawks_data %>%
  mutate(
    eFG_Percentage = (fgmPerGameTeam + 0.5 * fg3mPerGameTeam) / fgaPerGameTeam * 100,
    TOV_Percentage = tovPerGameTeam / (fgaPerGameTeam + 0.44 * ftaPerGameTeam + tovPerGameTeam) * 100,
    ORB_Percentage = orbPerGameTeam / (orbPerGameTeam + drbPerGameOpponent) * 100,
    FT_FGA = ftmPerGameTeam / fgaPerGameTeam * 100
  ) %>%
  select(yearSeason, eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA)

# Display results
hawks_four_factors

# Compute league averages
league_avg <- nba_data_flat %>%
  group_by(yearSeason) %>%
  summarise(
    eFG_League_Avg = mean((fgmPerGameTeam + 0.5 * fg3mPerGameTeam) / fgaPerGameTeam * 100, na.rm = TRUE),
    TOV_League_Avg = mean(tovPerGameTeam / (fgaPerGameTeam + 0.44 * ftaPerGameTeam + tovPerGameTeam) * 100, na.rm = TRUE),
    ORB_League_Avg = mean(orbPerGameTeam / (orbPerGameTeam + drbPerGameOpponent) * 100, na.rm = TRUE),
    FT_FGA_League_Avg = mean(ftmPerGameTeam / fgaPerGameTeam * 100, na.rm = TRUE)
  )

# Display results
league_avg

# Merge Hawks data with league averages
hawks_vs_league <- left_join(hawks_four_factors, league_avg, by = "yearSeason")

# Display merged data
hawks_vs_league

# Reshape data for visualization
hawks_vs_league_long <- hawks_vs_league %>%
  pivot_longer(cols = c(eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA, 
                        eFG_League_Avg, TOV_League_Avg, ORB_League_Avg, FT_FGA_League_Avg),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(
    Team = ifelse(Metric %in% c("eFG_Percentage", "TOV_Percentage", "ORB_Percentage", "FT_FGA"), 
                  "Hawks", "League Average"),
    Metric = case_when(
      Metric %in% c("eFG_Percentage", "eFG_League_Avg") ~ "eFG%",
      Metric %in% c("TOV_Percentage", "TOV_League_Avg") ~ "TOV%",
      Metric %in% c("ORB_Percentage", "ORB_League_Avg") ~ "ORB%",
      Metric %in% c("FT_FGA", "FT_FGA_League_Avg") ~ "FT/FGA"
    )
  )

# Display transformed data
hawks_vs_league_long

library(ggplot2)

# Create bar chart
ggplot(hawks_vs_league_long, aes(x = factor(yearSeason), y = Value, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Four-Factor Analysis: Atlanta Hawks vs. League Average",
    subtitle = "Comparison across multiple seasons",
    x = "Season",
    y = "Percentage",
    fill = "Team"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Hawks" = "darkred", "League Average" = "gray50")) +
  geom_text(aes(label = paste0(round(Value, 1), "%")),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5)

# Load required libraries
library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)

# Function to scrape Hawks game data for a given season
scrape_hawks_games <- function(year) {
  url <- paste0("https://www.basketball-reference.com/teams/ATL/", year, "_games.html") # Correct URL format
  
  # Read the webpage
  page <- read_html(url)
  
  # Extract the first table
  table <- page %>%
    html_node("table") %>%
    html_table(fill = TRUE)
  
  # Clean column names
  colnames(table) <- make.names(colnames(table))
  
  # Add season column
  table$Season <- year
  
  return(table)
}

# Scrape data for 2020-2024
hawks_games_2020 <- scrape_hawks_games(2020)
hawks_games_2021 <- scrape_hawks_games(2021)
hawks_games_2022 <- scrape_hawks_games(2022)
hawks_games_2023 <- scrape_hawks_games(2023)
hawks_games_2024 <- scrape_hawks_games(2024)

# Combine all seasons into one dataframe
hawks_games <- bind_rows(hawks_games_2020, hawks_games_2021, hawks_games_2022, hawks_games_2023, hawks_games_2024)

# Display first few rows
head(hawks_games)

str(hawks_games)  
colnames(hawks_games)  
dim(hawks_games)  
unique(hawks_games$Season)  

write.csv(hawks_games, "hawks_game_logs.csv", row.names = FALSE)

# Merge Hawks' four-factor data with actual wins
hawks_win_model <- hawks_vs_league %>%
  inner_join(hawks_games %>% select(Season, wins = W), by = c("yearSeason" = "Season"))

# Verify data structure
head(hawks_win_model)
str(hawks_win_model)

hawks_win_model$wins <- as.numeric(hawks_win_model$wins)
str(hawks_win_model)  # Check data types

colSums(is.na(hawks_win_model))  # Count missing values in each column

str(hawks_win_model)  # Check data structure
hawks_win_model$wins <- as.numeric(hawks_win_model$wins)  # Convert if necessary

table(hawks_win_model$wins)

win_model <- lm(wins ~ eFG_Percentage + TOV_Percentage + ORB_Percentage + FT_FGA, data = hawks_win_model)
summary(win_model)

# Load required package
library(car)

# Define the regression model
win_model <- lm(wins ~ eFG_Percentage + TOV_Percentage + ORB_Percentage + FT_FGA +
                  eFG_League_Avg + TOV_League_Avg + ORB_League_Avg + FT_FGA_League_Avg, 
                data = hawks_win_model)

# Calculate VIF
vif_values <- vif(win_model)

# Display VIF values
print(vif_values)

cor_matrix <- cor(hawks_win_model[, c("eFG_Percentage", "TOV_Percentage", "ORB_Percentage", 
                                      "FT_FGA", "eFG_League_Avg", "TOV_League_Avg", 
                                      "ORB_League_Avg", "FT_FGA_League_Avg")], use = "complete.obs")

print(cor_matrix)

library(car)
vif_values <- vif(win_model)
print(vif_values)

win_model <- lm(wins ~ eFG_Percentage + TOV_Percentage + ORB_Percentage + FT_FGA, 
                data = hawks_win_model)

summary(win_model)

vif_values <- vif(win_model)
print(vif_values)

win_model <- lm(wins ~ TOV_Percentage + ORB_Percentage + FT_FGA, 
                data = hawks_win_model)

vif_values <- vif(win_model)
print(vif_values)

win_model <- lm(wins ~ TOV_Percentage, data = hawks_win_model)
summary(win_model)

hawks_win_model <- distinct(hawks_win_model)
hawks_win_model %>%
  select(yearSeason, eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA, predicted_wins) %>%
  distinct()

hawks_win_model %>%
  select(yearSeason, wins, predicted_wins) %>%
  distinct()

library(ggplot2)

ggplot(hawks_win_model, aes(x = factor(yearSeason))) +
  geom_bar(aes(y = wins, fill = "Actual Wins"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = predicted_wins, fill = "Predicted Wins"), stat = "identity", position = "dodge") +
  labs(
    title = "Actual vs. Predicted Wins: Atlanta Hawks",
    subtitle = "Comparison across multiple seasons",
    x = "Season",
    y = "Wins",
    fill = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Actual Wins" = "darkred", "Predicted Wins" = "gray50")) 

hawks_win_model <- hawks_win_model %>%
  group_by(yearSeason) %>%
  summarise(
    eFG_Percentage = mean(eFG_Percentage, na.rm = TRUE),
    TOV_Percentage = mean(TOV_Percentage, na.rm = TRUE),
    ORB_Percentage = mean(ORB_Percentage, na.rm = TRUE),
    FT_FGA = mean(FT_FGA, na.rm = TRUE),
    wins = max(wins)  # Ensures only one value per season
  )

hawks_win_model %>% select(yearSeason, wins) %>% distinct()

win_model <- lm(wins ~ eFG_Percentage + TOV_Percentage + ORB_Percentage + FT_FGA, 
                data = hawks_win_model)
summary(win_model)

hawks_win_model <- hawks_win_model %>%
  mutate(predicted_wins = predict(win_model, newdata = hawks_win_model))

colSums(is.na(hawks_win_model))  # Check for NAs
hawks_win_model <- na.omit(hawks_win_model)  # Remove any rows with missing values

cor_matrix <- cor(hawks_win_model %>% select(eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA), use = "complete.obs")
print(cor_matrix)

win_model <- lm(wins ~ eFG_Percentage + ORB_Percentage + FT_FGA, 
                data = hawks_win_model)
summary(win_model)

library(car)
vif_values <- vif(win_model)
print(vif_values)

hawks_win_model <- hawks_win_model %>%
  mutate(predicted_wins = predict(win_model, newdata = hawks_win_model))

# Verify predictions
hawks_win_model %>% select(yearSeason, wins, predicted_wins) %>% distinct()

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Compute the year-over-year trends for each factor
hawks_vs_league_long <- hawks_vs_league %>%
  pivot_longer(cols = c(eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA, 
                        eFG_League_Avg, TOV_League_Avg, ORB_League_Avg, FT_FGA_League_Avg),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(
    Team = ifelse(Metric %in% c("eFG_Percentage", "TOV_Percentage", "ORB_Percentage", "FT_FGA"), 
                  "Hawks", "League Average"),
    Metric = case_when(
      Metric %in% c("eFG_Percentage", "eFG_League_Avg") ~ "eFG%",
      Metric %in% c("TOV_Percentage", "TOV_League_Avg") ~ "TOV%",
      Metric %in% c("ORB_Percentage", "ORB_League_Avg") ~ "ORB%",
      Metric %in% c("FT_FGA", "FT_FGA_League_Avg") ~ "FT/FGA"
    )
  )

# Create line plot for year-over-year trends
ggplot(hawks_vs_league_long, aes(x = factor(yearSeason), y = Value, color = Team, group = interaction(Team, Metric))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Year-Over-Year Trends: Hawks vs. League Average",
    subtitle = "Comparison of Four Factors Over the Past Five Seasons",
    x = "Season",
    y = "Percentage",
    color = "Team"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Hawks" = "darkred", "League Average" = "gray50"))

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Compute Four-Factor Metrics for the Hawks
hawks_four_factors <- hawks_data %>%
  mutate(
    eFG_Percentage = (fgmPerGameTeam + 0.5 * fg3mPerGameTeam) / fgaPerGameTeam * 100,
    TOV_Percentage = tovPerGameTeam / (fgaPerGameTeam + 0.44 * ftaPerGameTeam + tovPerGameTeam) * 100,
    ORB_Percentage = orbPerGameTeam / (orbPerGameTeam + drbPerGameOpponent) * 100,
    FT_FGA = ftmPerGameTeam / fgaPerGameTeam * 100
  ) %>%
  select(yearSeason, eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA)

# Transform data for visualization
hawks_four_factors_long <- hawks_four_factors %>%
  pivot_longer(cols = c(eFG_Percentage, TOV_Percentage, ORB_Percentage, FT_FGA),
               names_to = "Metric", values_to = "Value")

# Create the line plot
ggplot(hawks_four_factors_long, aes(x = factor(yearSeason), y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Four-Factor Performance of the Hawks (2020-2024)",
    subtitle = "Tracking eFG%, TOV%, ORB%, and FT/FGA over five seasons",
    x = "Season",
    y = "Percentage",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Extract the predicted wins for the 2023 season
predicted_wins_2023 <- hawks_win_model %>%
  filter(yearSeason == 2023) %>%
  select(yearSeason, wins, predicted_wins) %>%
  distinct()

# Display results
predicted_wins_2023

# Fit the updated regression model
win_model <- lm(wins ~ eFG_Percentage + ORB_Percentage + FT_FGA, 
                data = hawks_win_model)

# Display model summary
summary(win_model)

# Check for multicollinearity using Variance Inflation Factor (VIF)
library(car)
vif_values <- vif(win_model)
print(vif_values)

# Generate predicted wins based on the model
hawks_win_model <- hawks_win_model %>%
  mutate(predicted_wins = predict(win_model, newdata = hawks_win_model))

# Verify predictions for each season
hawks_win_model %>% select(yearSeason, wins, predicted_wins) %>% distinct()

# Extract the actual and predicted wins for the 2024 season
predicted_wins_2023 <- hawks_win_model %>%
  filter(yearSeason == 2023) %>%
  select(yearSeason, wins, predicted_wins) %>%
  distinct()

# Display the results
predicted_wins_2023

# Load necessary library
library(ggplot2)

# Convert data into a long format for plotting
hawks_wins_2023 <- predicted_wins_2023 %>%
  pivot_longer(cols = c(wins, predicted_wins), names_to = "Type", values_to = "Value")

# Generate the bar chart
ggplot(hawks_wins_2023, aes(x = factor(yearSeason), y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(
    title = "Actual vs. Predicted Wins: Atlanta Hawks (2023)",
    subtitle = "Comparison of Hawks' actual wins vs. model-predicted wins for 2023",
    x = "Season",
    y = "Wins",
    fill = "Win Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("wins" = "darkred", "predicted_wins" = "gray50")) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 5)

hawks_win_model %>%
  select(yearSeason, wins) %>%
  distinct() %>%
  arrange(yearSeason)

table(hawks_win_model$yearSeason)

head(hawks_win_model)

team_records <- bref_team_results(team = "")

team_records <- bref_team_results(team = "Atlanta Hawks", seasons = c(2020, 2021, 2022, 2023, 2024))

library(nbastatR)

# Load full game data for the Hawks
hawks_games <- teams_games(
  team = "Atlanta Hawks", 
  seasons = c(2020, 2021, 2022, 2023, 2024)
)

# View the first few rows
head(hawks_games)

install.packages("nbastatR")
library(nbastatR)

hawks_games <- load_nba_team_box(season = 2023, team = "Atlanta Hawks")
head(hawks_games)

install.packages("hoopR")
library(hoopR)

hawks_games <- load_nba_team_box(season = 2023, team = "Atlanta Hawks")
head(hawks_games)

hawks_2023_wins <- hawks_games %>%
  filter(team_name == "Atlanta Hawks") %>%  # Ensure filtering for Hawks
  mutate(win = ifelse(team_score > opponent_score, 1, 0)) %>%  # Identify wins
  summarise(total_wins = sum(win))

print(hawks_2023_wins)

library(dplyr)

# Ensure dplyr is loaded
library(dplyr)

# Check column names to ensure correct filtering
colnames(hawks_games)

# Compute the Hawks' actual wins for 2023
hawks_2023_wins <- hawks_games %>%
  filter(team_display_name == "Atlanta Hawks") %>%  # Adjust column name if necessary
  mutate(win = ifelse(team_score > opponent_score, 1, 0)) %>%  # Identify wins
  summarise(total_wins = sum(win))  # Sum total wins

# Display the actual wins
print(hawks_2023_wins)

hawks_2023_wins <- hawks_games %>%
  filter(team_display_name == "Atlanta Hawks") %>% # Adjust column name if necessary
  mutate(win = ifelse(team_score > opponent_team_score, 1, 0)) %>% # Identify wins
  summarise(total_wins = sum(win)) # Sum total wins

print(hawks_2023_wins)

hawks_win_model <- hawks_win_model %>%
  mutate(predicted_wins = predict(win_model, newdata = hawks_win_model))

hawks_win_model %>% filter(yearSeason == 2023) %>% select(yearSeason, wins, predicted_wins)

summary(hawks_win_model$eFG_Percentage)
summary(hawks_win_model$ORB_Percentage)

# Check if any values are missing
sum(is.na(hawks_win_model$eFG_Percentage))
sum(is.na(hawks_win_model$ORB_Percentage))

# Look at a few rows of data
head(hawks_win_model)

cor(hawks_win_model %>% select(eFG_Percentage, ORB_Percentage, FT_FGA, TOV_Percentage))

library(car)
vif_values <- vif(win_model)
print(vif_values)

hawks_win_model <- hawks_win_model %>%
  mutate(predicted_wins = predict(win_model, newdata = hawks_win_model))

hawks_win_model %>% filter(yearSeason == 2023) %>%
  select(yearSeason, wins, predicted_wins)

# Install hoopR if you haven't already
install.packages("hoopR")

# Load the necessary library
library(hoopR)
library(dplyr)

# Load player box scores for multiple seasons
hawks_players <- load_nba_player_box(seasons = 2020:2024) %>%
  filter(team_name == "Atlanta Hawks")

# View the first few rows
head(hawks_players)

# Install and load necessary libraries
install.packages("hoopR")
library(hoopR)
library(dplyr)

# Retrieve player statistics for the Hawks from 2020 to 2024
hawks_players <- load_nba_player_box(seasons = c(2020:2024)) %>%
  filter(team_name == "Atlanta Hawks")

# View first few rows
head(hawks_players)

hawks_players_cleaned <- hawks_players %>%
  select(player_name, season, team_name, points, assists, rebounds, 
         field_goals_made, field_goals_attempted, three_point_pct, 
         free_throw_pct, usage_pct, offensive_rating, defensive_rating)

# Display cleaned data
head(hawks_players_cleaned)

hawks_players_cleaned <- hawks_players %>%
  select(player_name = athlete_display_name, 
         season, 
         team_name, 
         points, 
         assists, 
         rebounds, 
         field_goals_made, 
         field_goals_attempted, 
         three_point_field_goals_attempted, 
         free_throws_made, 
         free_throws_attempted, 
         steals, 
         blocks, 
         plus_minus)

# View the cleaned data
head(hawks_players_cleaned)

hawks_players_cleaned <- hawks_players_cleaned %>%
  mutate(plus_minus = as.numeric(plus_minus))

hawks_player_summary <- hawks_players_cleaned %>%
  group_by(player_name, season) %>%
  summarise(
    avg_points = mean(points, na.rm = TRUE),
    avg_assists = mean(assists, na.rm = TRUE),
    avg_rebounds = mean(rebounds, na.rm = TRUE),
    avg_fg_made = mean(field_goals_made, na.rm = TRUE),
    avg_fg_attempted = mean(field_goals_attempted, na.rm = TRUE),
    avg_3pa = mean(three_point_field_goals_attempted, na.rm = TRUE),
    avg_ft_made = mean(free_throws_made, na.rm = TRUE),
    avg_ft_attempted = mean(free_throws_attempted, na.rm = TRUE),
    avg_steals = mean(steals, na.rm = TRUE),
    avg_blocks = mean(blocks, na.rm = TRUE),
    avg_plus_minus = mean(plus_minus, na.rm = TRUE)
  ) %>%
  ungroup()

# View summarized data
head(hawks_player_summary)

# Load required library
library(scales)

# Select numeric columns for normalization
hawks_player_normalized <- hawks_player_summary %>%
  mutate(across(c(avg_points, avg_assists, avg_rebounds, avg_fg_made, avg_fg_attempted, 
                  avg_3pa, avg_ft_made, avg_ft_attempted, avg_steals, avg_blocks, avg_plus_minus), 
                ~ rescale(.))) # Rescale between 0 and 1

# View normalized data
head(hawks_player_normalized)

# Check for missing values
colSums(is.na(hawks_player_summary))

# Check for Inf values
sapply(hawks_player_summary, function(x) sum(is.infinite(x)))

# Replace Inf values with NA, then fill with the column mean
hawks_player_summary[is.infinite(hawks_player_summary)] <- NA
hawks_player_summary <- hawks_player_summary %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

str(hawks_players_summary)

ls()
str(hawks_player_summary)

colSums(is.na(hawks_player_summary))

sapply(hawks_player_summary, function(x) sum(is.infinite(x)))
hawks_player_summary[sapply(hawks_player_summary, is.infinite)] <- NA

hawks_player_summary <- hawks_player_summary %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))
colSums(is.na(hawks_player_summary))
hawks_player_summary <- hawks_player_summary %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

library(scales)

hawks_player_normalized <- hawks_player_summary %>%
  mutate(across(where(is.numeric), ~ rescale(.)))  # Rescale between 0 and 1

# View the normalized dataset
head(hawks_player_normalized)

colSums(is.na(hawks_player_normalized))

hawks_player_normalized_clean <- hawks_player_normalized %>%
  select(where(~ !all(is.na(.)) & var(., na.rm = TRUE) > 0))

# Check dimensions
dim(hawks_player_normalized_clean)

set.seed(123) # For reproducibility
hawks_clusters <- kmeans(hawks_player_normalized[, -c(1,2)], centers = 4, nstart = 25)

# Add cluster assignments to the dataset
hawks_player_normalized$cluster <- as.factor(hawks_clusters$cluster)

# View first few rows
head(hawks_player_normalized)

# Create a bar chart comparing actual vs. predicted wins
ggplot(hawks_win_model_2023, aes(x = factor(yearSeason))) +
  geom_bar(aes(y = wins, fill = "Actual Wins"), stat = "identity", position = "dodge", width = 0.4) +
  geom_bar(aes(y = predicted_wins, fill = "Predicted Wins"), stat = "identity", position = "dodge", width = 0.4) +
  scale_fill_manual(values = c("Actual Wins" = "darkred", "Predicted Wins" = "steelblue")) +
  labs(
    title = "Atlanta Hawks: Actual vs. Predicted Wins (2023)",
    x = "Season",
    y = "Wins",
    fill = "Legend"
  ) +
  theme_minimal()

write.csv(hawks_data, "hawks_team_data_2020_2024.csv", row.names = FALSE)
write.csv(hawks_four_factors, "hawks_four_factors_2020_2024.csv", row.names = FALSE)
write.csv(league_avg, "league_four_factors_2020_2024.csv", row.names = FALSE)
write.csv(hawks_vs_league, "hawks_vs_league_comparison.csv", row.names = FALSE)
write.csv(hawks_games, "hawks_game_logs_2020_2024.csv", row.names = FALSE)
write.csv(hawks_win_model, "hawks_win_model_data.csv", row.names = FALSE)
write.csv(hawks_players, "hawks_player_statistics_2020_2024.csv", row.names = FALSE)
write.csv(hawks_player_summary, "hawks_player_summary_2020_2024.csv", row.names = FALSE)
write.csv(hawks_player_normalized, "hawks_player_normalized_2020_2024.csv", row.names = FALSE)
write.csv(predicted_wins_2023, "hawks_actual_vs_predicted_wins_2023.csv", row.names = FALSE)
