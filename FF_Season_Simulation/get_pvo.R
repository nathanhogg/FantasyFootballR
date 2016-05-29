get_pvo <- function() {
  
  library(rvest)
  library(plyr)
  source("get_team_lookup.R")
  
  # QB player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=1")
  pvo_qb <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_qb <- pvo_qb[-(1:2), -(2:19)]
  colnames(pvo_qb) <- c("Team", "Avg")
  pvo_qb$Avg <- as.numeric(pvo_qb$Avg)
  pvo_qb$QB_ratio <- pvo_qb$Avg / mean(pvo_qb$Avg)
  pvo_qb$Team <- as.character(lapply(pvo_qb$Team, FUN = function(x) substr(x, 1, nchar(x) - 7)))
  pvo_qb <- pvo_qb[order(pvo_qb$Team), ]
  
  # RB player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=2")
  pvo_rb <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_rb <- pvo_rb[-(1:2), -(2:19)]
  colnames(pvo_rb) <- c("Team", "Avg")
  pvo_rb$Avg <- as.numeric(pvo_rb$Avg)
  pvo_rb$RB_ratio <- pvo_rb$Avg / mean(pvo_rb$Avg)
  pvo_rb$Team <- as.character(lapply(pvo_rb$Team, FUN = function(x) substr(x, 1, nchar(x) - 7)))
  pvo_rb <- pvo_rb[order(pvo_rb$Team), ]

  # WR player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=3")
  pvo_wr <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_wr <- pvo_wr[-(1:2), -(2:19)]
  colnames(pvo_wr) <- c("Team", "Avg")
  pvo_wr$Avg <- as.numeric(pvo_wr$Avg)
  pvo_wr$WR_ratio <- pvo_wr$Avg / mean(pvo_wr$Avg)
  pvo_wr$Team <- as.character(lapply(pvo_wr$Team, FUN = function(x) substr(x, 1, nchar(x) - 7)))
  pvo_wr <- pvo_wr[order(pvo_wr$Team), ]

  # TE player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=4")
  pvo_te <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_te <- pvo_te[-(1:2), -(2:19)]
  colnames(pvo_te) <- c("Team", "Avg")
  pvo_te$Avg <- as.numeric(pvo_te$Avg)
  pvo_te$TE_ratio <- pvo_te$Avg / mean(pvo_te$Avg)
  pvo_te$Team <- as.character(lapply(pvo_te$Team, FUN = function(x) substr(x, 1, nchar(x) - 7)))
  pvo_te <- pvo_te[order(pvo_te$Team), ]
  
  # K player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=5")
  pvo_k <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_k <- pvo_k[-c(1,2,35), -(2:11)]
  colnames(pvo_k) <- c("Team", "Avg")
  pvo_k$Avg <- as.numeric(pvo_k$Avg)
  pvo_k$K_ratio <- pvo_k$Avg / mean(pvo_k$Avg)
  pvo_k$Team <- as.character(lapply(pvo_k$Team, FUN = function(x) substr(x, 1, nchar(x) - 6)))
  pvo_k <- pvo_k[order(pvo_k$Team), ]
  
  # D/ST player vs. opponent
  pvo_url <- read_html("http://games.espn.go.com/ffl/pointsagainst?&positionId=16")
  pvo_dst <- html_table(html_nodes(pvo_url, "table")[[2]], fill = TRUE)
  pvo_dst <- pvo_dst[-(1:2), -(2:19)]
  colnames(pvo_dst) <- c("Team", "Avg")
  pvo_dst$Avg <- as.numeric(pvo_dst$Avg)
  pvo_dst$DST_ratio <- pvo_dst$Avg / mean(pvo_dst$Avg)
  pvo_dst$Team <- as.character(lapply(pvo_dst$Team, FUN = function(x) substr(x, 1, nchar(x) - 9)))
  pvo_dst <- pvo_dst[order(pvo_dst$Team), ]
  
  # Merge positional pvo DF into one
  pvo <- cbind.data.frame(pvo_qb$Team, pvo_qb$QB_ratio, 
                          pvo_rb$RB_ratio, pvo_wr$WR_ratio, 
                          pvo_te$TE_ratio, pvo_k$K_ratio,
                          pvo_dst$DST_ratio)
  colnames(pvo) <- c("Team_Name", "QB", "RB", "WR", "TE", "K", "DST")
  pvo$Team_Name <- as.character(pvo$Team_Name)
  
  # Change team name to team abbreviation
  team_lookup <- get_team_lookup()
  pvo <- join(pvo, team_lookup, by="Team_Name")
  
  # Tidy data frame and add BYE row
  pvo <- pvo[ , c(8, 1, 2, 3, 4, 5, 6, 7)]
  
  pvo_bye <- list("BYE", "BYE", 0, 0, 0, 0, 0, 0)
  pvo_bye <- data.frame(pvo_bye)
  colnames(pvo_bye) <- colnames(pvo)
  
  pvo <- rbind(pvo, pvo_bye)
  colnames(pvo) <- c("Team", "Team_Name", "QB", "RB", "WR", "TE", "K", "DST")
  
  return(pvo)
  
}
