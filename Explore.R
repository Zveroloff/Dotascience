library("jsonlite")
#library("httr")
library("gbm")
library("caret")

game.details <- lapply(readLines(file("Data/selected_team_match_info.jsonlines", "r", blocking = F)), fromJSON)
game.stats <- read.csv("Data/selected_team_matches.csv")
game.stats.mod <- game.stats[,c("radiant","dire","winner")]
game.stats.mod[game.stats.mod$winner == "RADIANT", "team.winner" ] <- as.character(game.stats.mod[game.stats.mod$winner == "RADIANT", "radiant" ])
game.stats.mod[game.stats.mod$winner == "DIRE", "team.winner" ] <- as.character(game.stats.mod[game.stats.mod$winner == "DIRE", "dire" ])
game.stats.mod[game.stats.mod$winner == "RADIANT", "team.loser" ] <- as.character(game.stats.mod[game.stats.mod$winner == "RADIANT", "dire" ])
game.stats.mod[game.stats.mod$winner == "DIRE", "team.loser" ] <- as.character(game.stats.mod[game.stats.mod$winner == "DIRE", "radiant" ])
game.stats.mod[,"win"] <- 1
team.winner <- game.stats.mod$team.loser
team.loser <- game.stats.mod$team.winner
ttt <- data.frame(cbind(team.winner, team.loser))
ttt[game.stats.mod$winner == "DIRE","side"] <- "RADIANT"
ttt[game.stats.mod$winner == "RADIANT","side"] <- "DIRE"
ttt[,"win"] <- "0"
game.stats.mod <- game.stats.mod[,c(4,5,3,6)]
names(game.stats.mod) <- names(ttt)
game.stats.feat <- rbind(game.stats.mod, ttt)
game.stats.feat[,"team.winner"] <- as.factor(game.stats.feat[,"team.winner"])
game.stats.feat[,"team.loser"] <- as.factor(game.stats.feat[,"team.loser"])
game.stats.feat[,"side"] <- as.factor(game.stats.feat[,"side"])
game.stats.feat[,"win"] <- as.numeric(game.stats.feat[,"win"])
names(game.stats.feat) <- c("host","competitor","side","win")

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)
gbmFit <- train(win ~ ., data = game.stats.feat,
                method = "gbm",
                trControl = fitControl)