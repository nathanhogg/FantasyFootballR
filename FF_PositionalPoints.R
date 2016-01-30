# Script Name:  FF_PositionalPoints.R
#      Author:  Nathan Hogg
#  Orig. Date:  1/30/2016
#    Overview:  This script shows how fantasy teams scored points at different
#               positions during a season
# 
# Inputs:
#   1) league_id - ESPN league ID, can be found from the URL of the league page
#   2) league_size - Number of fantasy teams in the league
#   3) s_qb, s_rb, etc. - Number of starters at each position for the league
#   4) weeks - Number of weeks in the season (or number of weeks played so far)
#
# Outputs:
#   1) pos_pts - data frame with fantasy teams as rows and positions as columns

rm(list = ls())

# Packages required for this script:
library(rvest)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)
library(DT)

# User inputs:
league_id <- 30173
league_size <- 10
s_qb <- 1
s_rb <- 2
s_wr <- 2 
s_te <- 1
s_fx <- 2
s_ds <- 1
s_k  <- 0
weeks <- 13

# Create a data frame for storing the results of each week
pos_pts <- data.frame(x = character(league_size), y = numeric(league_size), 
                      z = numeric(league_size), a = numeric(league_size), 
                      b = numeric(league_size), c = numeric(league_size),
                      d = numeric(league_size), e = numeric(league_size), 
                      f = numeric(league_size), g = numeric(league_size), 
                      stringsAsFactors = FALSE)

# Rename the columns in pos_pts
colnames(pos_pts) <- c("Team", "QB_Pts", "RB_Str_Pts", "WR_Str_Pts", 
                       "TE_Pts", "Flex_Pts", "RB_All_Pts", "WR_All_Pts",
                       "DST_Pts", "K_Pts")
  
# Create a loop for each team in the league
for (a in 1:league_size) {

  # Create a loop for each week in the season
  for (i in 1:weeks) {
  
    # Obtain tables for starters and bench players
    url <- read_html(paste0("http://games.espn.go.com/ffl/boxscorequick?",
                            "leagueId=", league_id, "&teamId=", a, 
                            "&scoringPeriodId=", i, 
                            "&seasonId=2015&view=scoringperiod&version=quick"))
    
    # Get a table of starters for the current team/week
    starters <- html_table(html_nodes(url, "table")[[2]], fill = TRUE)
    
    # If this is the first week of the loop, add the team name to pos_pts
    if (i == 1) {
      pos_pts$Team[a] <- substr(as.character(starters[1,1]), 1, 
                                nchar(as.character(starters[1,1])) - 10)
    }
    
    # Perform data tidying on starters table
    colnames(starters) <- starters[3,]
    starters <- starters[-c(1,2,3), -c(3, 4, 6:ncol(starters))]
    starters$PTS <- as.numeric(starters$PTS)
    starters <- separate(starters, 'PLAYER, TEAM POS', c("Player", "TeamPos"), 
                         sep = ", ", extra = "drop")
    starters$TeamPos <- gsub("\\s+", " ", starters$TeamPos)
    starters <- separate(starters, 'TeamPos', c("Team", "Pos"), 
                          sep = " ", extra = "drop")
    starters$Pos[is.na(starters$Pos)] <- "D/ST"
    starters$PTS[is.na(starters$PTS)] <- 0
    
    # Add the current week points by position to the total points for 
    # the current team
    pos_pts$QB_Pts[a] <- as.numeric(pos_pts$QB_Pts[a]) + 
                         starters$PTS[starters$SLOT == "QB"]
    pos_pts$RB_Str_Pts[a] <- as.numeric(pos_pts$RB_Str_Pts[a]) + 
                             sum(starters$PTS[starters$SLOT == "RB"])
    pos_pts$WR_Str_Pts[a] <- as.numeric(pos_pts$WR_Str_Pts[a]) + 
                             sum(starters$PTS[starters$SLOT == "WR"])
    pos_pts$TE_Pts[a] <- as.numeric(pos_pts$TE_Pts[a]) + 
                         sum(starters$PTS[starters$SLOT == "TE"])
    pos_pts$RB_All_Pts[a] <- as.numeric(pos_pts$RB_All_Pts[a]) + 
                             sum(starters$PTS[starters$Pos == "RB"])
    pos_pts$WR_All_Pts[a] <- as.numeric(pos_pts$WR_All_Pts[a]) + 
                             sum(starters$PTS[starters$Pos == "WR"])
    
    # Add up flex points if the league has flex
    if (s_fx > 0) {
      pos_pts$Flex_Pts[a] <- as.numeric(pos_pts$Flex_Pts[a]) + 
        sum(starters$PTS[starters$SLOT == "FLEX"])
    }
    
    # Add up defense points if the league has defenses
    if (s_ds > 0) {
      pos_pts$DST_Pts[a] <- as.numeric(pos_pts$DST_Pts[a]) + 
        sum(starters$PTS[starters$SLOT == "D/ST"])
    }
    
    # Add up kicker points if the league has kickers
    if (s_k > 0) {
      pos_pts$K_Pts[a] <- as.numeric(pos_pts$K_Pts[a]) + 
        sum(starters$PTS[starters$SLOT == "K"])
    }
    
    # Display the current week and team so progress is known
    print(paste0("Team: ", a, "   Week: ", i))
  }
}

# Update the colnames so they aren't underscored
colnames(pos_pts) <- c("Team", "QB Pts", "RB Starter Pts", "WR Starter Pts",
                       "TE Pts", "Flex Pts", "All RB Pts", "All WR Pts", 
                       "D/ST Pts", "K Pts")

# Remove columns for positions the league doesn't use
if (s_fx == 0) {
  pos_pts <- pos_pts[ , -which(names(pos_pts) %in% c("Flex Pts"))]
}

if (s_ds == 0) {
  pos_pts <- pos_pts[ , -which(names(pos_pts) %in% c("D/ST Pts"))]
}

if (s_k == 0) {
  pos_pts <- pos_pts[ , -which(names(pos_pts) %in% c("K Pts"))]
}

# Print results into the viewer
datatable(pos_pts)
