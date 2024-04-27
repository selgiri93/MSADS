
library(dplyr)
library(tidyr)
library(readxl)
library(rpart)
library(rpart.plot)

# Load the datasets
nfl_teams <- read.csv("C://Users/santh/OneDrive/Documents/Syracuse/IST_707_Applied Machine Learning/PROJECT/nfl_teams.csv")
scores <- read.csv("C://Users/santh/OneDrive/Documents/Syracuse/IST_707_Applied Machine Learning/PROJECT/spreadspoke_scores.csv")


# Calculate win/loss for each game
scores <- scores %>%
  mutate(home_win = ifelse(score_home > score_away, 1, 0),
         away_win = ifelse(score_away > score_home, 1, 0))

home_wins <- scores %>%
  group_by(team_home) %>%
  summarise(HomeWins = sum(home_win))

away_wins <- scores %>%
  group_by(team_away) %>%
  summarise(AwayWins = sum(away_win))

home_wins <- home_wins %>% rename(team = team_home, HomeWins = HomeWins)
away_wins <- away_wins %>% rename(team = team_away, AwayWins = AwayWins)

win_percentages <- home_wins %>%
  full_join(away_wins, by = "team") %>%
  mutate(TotalWins = coalesce(HomeWins, 0) + coalesce(AwayWins, 0)) %>%
  left_join(scores %>% count(team_home) %>% rename(team = team_home, TotalGamesHome = n), by = "team") %>%
  left_join(scores %>% count(team_away) %>% rename(team = team_away, TotalGamesAway = n), by = "team") %>%
  mutate(TotalGames = coalesce(TotalGamesHome, 0) + coalesce(TotalGamesAway, 0),
         WinPercentage = ifelse(TotalGames > 0, (TotalWins / TotalGames) * 100, NA)) %>%
  select(team, TotalGames, TotalWins, WinPercentage)


# Merge nfl_teams dataset with win_percentages dataset
teams_with_win_pct <- merge(nfl_teams, win_percentages, by.x = "team_name", by.y = "team", all.x = TRUE)

# Write the file to local
write.csv(teams_with_win_pct, "C:/Users/santh/OneDrive/Documents/Syracuse/IST_707_Applied Machine Learning/PROJECT/teams_with_win_pct.csv", row.names = FALSE)

# Anova
decision_tree_model <- rpart(WinPercentage ~ ., data = teams_with_win_pct, method = "anova")

#Visualize the decision tree
rpart.plot(decision_tree_model)
summary(decision_tree_model)

### More Tuning of the Model ###

library(caret) 
predictors <- c("team_name", "TotalGames", "TotalWins") 
target <- "WinPercentage"

dataset <- teams_with_win_pct %>%
  select(all_of(c(predictors, target))) %>%
  na.omit() # Removing rows with NA values

# Cross-validation setup
set.seed(123) # For reproducibility
trainControl <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Training the model with cross-validation
model <- train(reformulate(predictors, target), data = dataset,
               method = "rpart",
               trControl = trainControl,
               tuneGrid = expand.grid(cp = seq(0.01, 0.1, by = 0.01)), 
               control = rpart.control(minsplit = 20, maxdepth = 5)) 

print(model)

rpart.plot(model$finalModel)
summary(model$finalModel)

importance <- varImp(model, scale = FALSE)
print(importance)

