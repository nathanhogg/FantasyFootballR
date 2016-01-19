# Script Name:  FF_FAAB.R
#      Author:  Nathan Hogg
#  Orig. Date:  1/13/2016
#    Overview:  This script seeks to analyze how FAAB dollars are spent in an
#               ESPN fantasy football league
# Inputs:
#   1) league_id - ESPN league ID, can be found from the URL of the league page
#   2) start_date, end_date - Dates to being and end scraping of FAAB report
#   2) qb_ref, rb_ref, etc. - Reference player for player valuation calculations
#
# Outputs:
#   1) faab_log - log of all FAAB activitiy in
#   2) faab_team - summary of faab_log by each fantasy team in the league
#   3) faab_pos - summary of faab_log by fantasy position

rm(list = ls())  # Clear any unwanted variables / lists / etc.

# Load the necessary packages for this program
library(rvest)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)
library(DT)
library(reshape2)
library(ggplot2)

# Input variables
league_id <- 367938  # Note: ESPN league must be "viewable to public"
start_date <- "2015/09/01"
end_date <- "2016/01/04"
qb_ref <- 12
rb_ref <- 36
wr_ref <- 40
te_ref <- 12

## SECTION 1: OBTAINING FAAB_LOG, A HISTORY OF SEASON FAAB BIDS --------------

# Create a list of every date between start_date and end_date
date_list <- seq(as.Date(start_date), as.Date(end_date), "days")
date_list <- gsub("-", "", date_list)  # format date in ESPN url format
n_dates <- length(date_list)  # count the number of dates in date_list

# Create empty data.frame to update every time new log information is scraped
faab_log <- data.frame(FF_Team = character(0), Player = character(0), 
                       Bid = character(0), Outcome = character(0), Date = character(0))

# Loop over all dates in the date_list and update faab_log as needed
for (i in 1:n_dates) {
  # Build url for the league and with the current date in the loop
  url_date <- read_html(paste0("http://games.espn.go.com/ffl/waiverreport?leagueId=", 
                               league_id, "&date=", date_list[i]))

  # Build table to test whether any transactions were processed on the current date
  test_table <- html_table(html_nodes(url_date, "table")[[1]], fill = TRUE)
  
  # If no trans occurred, ncol(test_table) will be 1; Otherwise, it will be > 1
  if (ncol(test_table) == 1) {
    # Print results of FAAB activity on the current date
    print(paste0(date_list[i], " - No transactions"))
    
  } else {
    # Build table with activity that occurred on the current date and format
    current_date_table <- html_table(html_nodes(url_date, "table")[[2]], fill = TRUE)
    current_date_table <- current_date_table[-c(1,2), -c(1)]  # remove rows/cols
    colnames(current_date_table) <- c("FF_Team", "Player", "Bid", "Outcome")
    current_date_table$Date <- date_list[i]  # Add the current date to the log
    
    # Merge the faab_log with the current_date_table
    faab_log <- rbind(faab_log, current_date_table)
    
    # Print results of FAAB activity on the current date
    print(paste0(date_list[i], " - ", nrow(current_date_table), " bid(s), ", length(unique(current_date_table$Player)), " transaction(s)"))
    
  }
  
  # Remove un-needed tables
  remove(current_date_table, test_table)
}

## SECTION 2: CLEANING UP FAAB_LOG --------------------------------------------

# Save the faab_log as a .csv file so it doesn't need to be re-downloaded later
save(faab_log, file = "faab_log.csv")  # load("faab_log.csv")

# Convert the bid column to numerical, and the date column to a date
faab_log$Bid <- as.character(faab_log$Bid)
faab_log$Bid <- substr(faab_log$Bid, 2, nchar(faab_log$Bid))
faab_log$Bid <- as.numeric(faab_log$Bid)
faab_log$Date <- as.Date(faab_log$Date, "%Y%m%d")

# Separate out player, team, and position into new columns
faab_log <- separate(faab_log, 'Player', c("Added_Player", "Added_TeamPos"), 
                     sep = ", ", extra = "drop")
faab_log$Added_Player <- gsub("\\*", "", faab_log$Added_Player)
faab_log$Added_TeamPos <- gsub("\\s+", " ", faab_log$Added_TeamPos)
faab_log <- separate(faab_log, 'Added_TeamPos', c("Added_Team", "Added_Pos"), 
                     sep = " ", extra = "drop")
faab_log$Added_Pos[is.na(faab_log$Added_Pos)] <- "D/ST"

# Add a Yes/No column for if a bid was successful
index <- c("Added", "Unsuc")
values <- c("Yes", "No")
faab_log$Success <- values[match(substr(faab_log$Outcome,1,5), index)]

# Add a Yes/No column for if a player was dropped during the transaction
faab_log$Drop[grepl("dropped", faab_log$Outcome) == TRUE] <- "Yes"
faab_log$Drop[grepl("dropped", faab_log$Outcome) == FALSE] <- "No"

# Add a column with the player dropped if dropped column = "Yes"
faab_log$Dropped_Player[faab_log$Drop == "Yes"] <- substr(faab_log$Outcome[faab_log$Drop == "Yes"], 
                                                                                regexpr("dropped", faab_log$Outcome[faab_log$Drop == "Yes"])[1] + 5, 
                                                                                nchar(faab_log$Outcome[faab_log$Drop == "Yes"]) - 12)
x <- colsplit(faab_log$Dropped_Player, " ", c("Crap", "Player_Name"))
faab_log$Player_Name <- x$Player_Name
faab_log <- faab_log[ , -c(10)]
remove(x)

# Split up dropped players, teams, and positions into their own columns
faab_log <- separate(faab_log, 'Player_Name', c("Dropped_Player", "Dropped_TeamPos"), sep = ", ", extra = "drop")
faab_log$Dropped_Player <- gsub("\\*", "", faab_log$Dropped_Player)
faab_log$Dropped_TeamPos <- gsub("\\s+", " ", faab_log$Dropped_TeamPos)
faab_log <- separate(faab_log, 'Dropped_TeamPos', c("Dropped_Team", "Dropped_Pos"), 
                           sep = " ", extra = "drop")

# Remove the double "D/ST" for defenses in the players column
faab_log$Added_Player[is.na(faab_log$Added_Team) == TRUE] <- substr(faab_log$Added_Player[is.na(faab_log$Added_Team) == TRUE], 1, 
                                                      nchar(faab_log$Added_Player[is.na(faab_log$Added_Team) == TRUE]) - 5)

## SECTION 3: ADDING PRO-FOOTBALL-REFERENCE TO FAAB_LOG -----------------------

# Get pro-football-reference fantasy table for the current season
url_pfr <- read_html("http://www.pro-football-reference.com/years/2015/fantasy.htm")
pfr <- html_table(html_nodes(url_pfr, "table")[[1]], fill = TRUE)

# Remove the extra headers that occur periodically and the first column
pfr <- pfr[-seq(31, nrow(pfr), 32), - c(1)]
pfr <- pfr[-seq(31, nrow(pfr), 31),]
pfr <- pfr[-c(1),]

# Name columns and remove miscellaneous symbols
colnames(pfr) <- c("Player", "Team", "Age", "G", "GS", "Pass_Cmp", "Pass_ATT", "Pass_Yds", "Pass_TD",
                   "Pass_Int", "Rush_Att", "Rush_Yds", "Rush_Y/A", "Rush_TD", "Rec_Tgt", "Rec", "Rec_Yds",
                   "Rec_Y/R", "Rec_TD", "Pos", "Fant_Pts", "DK_Pts", "FD_Pts", "VBD", "Pos_Rank", "Ov_Rank")
pfr$Player <- gsub("\\+", "", pfr$Player)

# Convert columns to numeric as needed
for (i in c(3:19,21:26)) {
  pfr[, i] <- as.numeric(pfr[, i])
}

# If Fant_Pts is NA, set it to 0
pfr$Fant_Pts[is.na(pfr$Fant_Pts)] <- 0

# Add Fant_PPG and Fant_PPGS columns
pfr$Fant_PPG <- pfr$Fant_Pts / pfr$G
pfr$Fant_PPGS <- pfr$Fant_Pts / pfr$GS

# If Fant_PPG/S is infinite, set it to 0
pfr$Fant_PPG[is.infinite(pfr$Fant_PPG)] <- 0
pfr$Fant_PPGS[is.infinite(pfr$Fant_PPGS)] <- 0

# Split up table by position
pfr_qb <- pfr[pfr$Pos == "QB", ]
pfr_rb <- pfr[pfr$Pos == "RB", ]
pfr_wr <- pfr[pfr$Pos == "WR", ]
pfr_te <- pfr[pfr$Pos == "TE", ]

# Find the Xth highest value for the Fant_Pts, Fant_PPG, and Fant_PPGS

  # QB VORP - use the 12th highest qb
  n_qb <- nrow(pfr_qb)
  qb_rp_pts <- sort(pfr_qb$Fant_Pts, partial = n_qb - qb_ref + 1)[n_qb - qb_ref + 1]
  qb_rp_ppg <- sort(pfr_qb$Fant_PPG, partial = n_qb - qb_ref + 1)[n_qb - qb_ref + 1]
  qb_rp_ppgs <- sort(pfr_qb$Fant_PPGS, partial = n_qb - qb_ref + 1)[n_qb - qb_ref + 1]
  pfr_qb$VORP_pts <- pfr_qb$Fant_Pts - qb_rp_pts
  pfr_qb$VORP_ppg <- pfr_qb$Fant_PPG - qb_rp_ppg
  pfr_qb$VORP_ppgs <- pfr_qb$Fant_PPGS - qb_rp_ppgs
  remove(n_qb, qb_rp_pts, qb_rp_ppg, qb_rp_ppgs)
  
  # RB VORP - use the 36th highest rb
  n_rb <- nrow(pfr_rb)
  rb_rp_pts <- sort(pfr_rb$Fant_Pts, partial = n_rb - rb_ref + 1)[n_rb - rb_ref + 1]
  rb_rp_ppg <- sort(pfr_rb$Fant_PPG, partial = n_rb - rb_ref + 1)[n_rb - rb_ref + 1]
  rb_rp_ppgs <- sort(pfr_rb$Fant_PPGS, partial = n_rb - rb_ref + 1)[n_rb - rb_ref + 1]
  pfr_rb$VORP_pts <- pfr_rb$Fant_Pts - rb_rp_pts
  pfr_rb$VORP_ppg <- pfr_rb$Fant_PPG - rb_rp_ppg
  pfr_rb$VORP_ppgs <- pfr_rb$Fant_PPGS - rb_rp_ppgs
  remove(n_rb, rb_rp_pts, rb_rp_ppg, rb_rp_ppgs)
  
  # WR VORP - use the 36th highest qb
  n_wr <- nrow(pfr_wr)
  wr_rp_pts <- sort(pfr_wr$Fant_Pts, partial = n_wr - wr_ref + 1)[n_wr - wr_ref + 1]
  wr_rp_ppg <- sort(pfr_wr$Fant_PPG, partial = n_wr - wr_ref + 1)[n_wr - wr_ref + 1]
  wr_rp_ppgs <- sort(pfr_wr$Fant_PPGS, partial = n_wr - wr_ref + 1)[n_wr - wr_ref + 1]
  pfr_wr$VORP_pts <- pfr_wr$Fant_Pts - wr_rp_pts
  pfr_wr$VORP_ppg <- pfr_wr$Fant_PPG - wr_rp_ppg
  pfr_wr$VORP_ppgs <- pfr_wr$Fant_PPGS - wr_rp_ppgs
  remove(n_wr, wr_rp_pts, wr_rp_ppg, wr_rp_ppgs)
  
  # TE VORP - use the 12th highest te
  n_te <- nrow(pfr_te)
  te_rp_pts <- sort(pfr_te$Fant_Pts, partial = n_te - te_ref + 1)[n_te - te_ref + 1]
  te_rp_ppg <- sort(pfr_te$Fant_PPG, partial = n_te - te_ref + 1)[n_te - te_ref + 1]
  te_rp_ppgs <- sort(pfr_te$Fant_PPGS, partial = n_te - te_ref + 1)[n_te - te_ref + 1]
  pfr_te$VORP_pts <- pfr_te$Fant_Pts - te_rp_pts
  pfr_te$VORP_ppg <- pfr_te$Fant_PPG - te_rp_ppg
  pfr_te$VORP_ppgs <- pfr_te$Fant_PPGS - te_rp_ppgs
  remove(n_te, te_rp_pts, te_rp_ppg, te_rp_ppgs)
  
# Recombine positional tables back into the big pfr table
pfr <- rbind(pfr_qb, pfr_rb, pfr_wr, pfr_te)
    
# Match VORP data with faab_log for both adds and drops
index <- pfr$Player
values_1 <- pfr$VORP_pts
values_2 <- pfr$VORP_ppg
values_3 <- pfr$VORP_ppgs

faab_log$Added_VORP_pts <- values_1[match(faab_log$Added_Player, index)]
faab_log$Added_VORP_ppg <- values_2[match(faab_log$Added_Player, index)]
faab_log$Added_VORP_ppgs <- values_3[match(faab_log$Added_Player, index)]

faab_log$Dropped_VORP_pts <- values_1[match(faab_log$Dropped_Player, index)]
faab_log$Dropped_VORP_ppg <- values_2[match(faab_log$Dropped_Player, index)]
faab_log$Dropped_VORP_ppgs <- values_3[match(faab_log$Dropped_Player, index)]

# If the bid wasn't successful, make the VORP columns NA
faab_log$Added_VORP_pts[faab_log$Success == "No"] <- NA
faab_log$Added_VORP_ppg[faab_log$Success == "No"] <- NA
faab_log$Added_VORP_ppgs[faab_log$Success == "No"] <- NA

## SECTION 4: CREATE OUTPUT TABLES TO SUMMARIZE THE DATA -------------------------

# Build a table to summarize faab_log by fantasy football team
faab_team <- data.frame(FF_Team = unique(faab_log$FF_Team))
n_team <- nrow(faab_team)

# Loop over each team and calculate relevant stats
for (i in 1:n_team) {
  # Add the amount of FAAB budget a team had remaining
  faab_team$Remaining[i] <- 100 - sum(faab_log$Bid[faab_log$FF_Team == faab_team[i,1]])
  
  # Add the number of bids and adds the team made
  faab_team$Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1])
  faab_team$Adds[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Success == "Yes")
  
  # Add stats on the dollar amounts bid 
  faab_team$Avg_Bid[i] <- round(mean(faab_log$Bid[faab_log$FF_Team == faab_team[i,1]]), digits = 2)
  faab_team$Avg_Add_Cost[i] <- round(mean(faab_log$Bid[faab_log$FF_Team == faab_team[i,1] & faab_log$Success == "Yes"]), digits = 2)
  faab_team$Max_Bid[i] <- max(faab_log$Bid[faab_log$FF_Team == faab_team[i,1]])
  faab_team$Med_Bid[i] <- median(faab_log$Bid[faab_log$FF_Team == faab_team[i,1]])
  
  # Add a breakdown for how the team bid on different positions
  faab_team$QB_Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_Pos == "QB")
  faab_team$RB_Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_Pos == "RB")
  faab_team$WR_Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_Pos == "WR")
  faab_team$TE_Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_Pos == "TE")
  faab_team$DST_Bids[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_Pos == "D/ST")
  
  # Show how much VORP and VORP_ppg the team added in their acquisitions
  faab_team$VORP_Added[i] <- sum(faab_log$Added_VORP_pts[faab_log$FF_Team == faab_team[i,1] & faab_log$Added_VORP_pts > 0 & !is.na(faab_log$Added_VORP_pts)])
  faab_team$VORP_PPG_Added[i] <- round(sum(faab_log$Added_VORP_ppg[faab_log$FF_Team == faab_team[i,1] & faab_log$Added_VORP_ppg > 0 & !is.na(faab_log$Added_VORP_ppg)]), digits = 2)
  
  # Show the number of players with positive VORP and VORP_ppg the team added
  faab_team$Pos_VORP_Adds[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_VORP_pts > 0 & faab_log$Success == "Yes" & !is.na(faab_log$Added_VORP_pts))
  faab_team$Pos_VORP_PPG_Adds[i] <- sum(faab_log$FF_Team == faab_team[i,1] & faab_log$Added_VORP_ppg > 0 & faab_log$Success == "Yes" & !is.na(faab_log$Added_VORP_ppg))
  
  # Show how much VORP and VORP_ppg the team dropped
  faab_team$VORP_Dropped[i] <- sum(faab_log$Dropped_VORP_pts[faab_log$FF_Team == faab_team[i,1] & faab_log$Dropped_VORP_pts > 0 & !is.na(faab_log$Dropped_VORP_pts)])
  faab_team$VORP_PPG_Dropped[i] <- round(sum(faab_log$Dropped_VORP_ppg[faab_log$FF_Team == faab_team[i,1] & faab_log$Dropped_VORP_ppg > 0 & !is.na(faab_log$Dropped_VORP_ppg)]), digits = 2)
}

# Build a table to look at how FAAB budget was spent on different positions
faab_pos <- data.frame(Position = unique(faab_log$Added_Pos[!is.na(faab_log$Added_Pos)]))
n_pos <- nrow(faab_pos)

# Loop over each position and calculate relevant stats
for (i in 1:n_pos) {
  # Add the count of bids and adds for the position
  faab_pos$Bids[i] <- sum(faab_log$Added_Pos == faab_pos[i,1])
  faab_pos$Adds[i] <- sum(faab_log$Added_Pos == faab_pos[i,1] & faab_log$Success == "Yes")
  
  # Add stats on the bid and add cost for the position
  faab_pos$Avg_Bid[i] <- round(mean(faab_log$Bid[faab_log$Added_Pos == faab_pos[i,1]]), digits = 2)
  faab_pos$Avg_Add_Cost[i] <- round(mean(faab_log$Bid[faab_log$Added_Pos == faab_pos[i,1] & faab_log$Success == "Yes"]), digits = 2)
  faab_pos$Max_Bid[i] <- max(faab_log$Bid[faab_log$Added_Pos == faab_pos[i,1]])
  faab_pos$Med_Bid[i] <- median(faab_log$Bid[faab_log$Added_Pos == faab_pos[i,1]])
  
  # Add the amount of VORP and VORP_ppg added by position
  faab_pos$VORP_Added[i] <- sum(faab_log$Added_VORP_pts[faab_log$Added_Pos == faab_pos[i,1] & faab_log$Added_VORP_pts > 0 & !is.na(faab_log$Added_VORP_pts)])
  faab_pos$VORP_PPG_Added[i] <- round(sum(faab_log$Added_VORP_ppg[faab_log$Added_Pos == faab_pos[i,1] & faab_log$Added_VORP_ppg > 0 & !is.na(faab_log$Added_VORP_ppg)]), digits = 2)

  # Add the number of players added with positive VORP and VORP_ppg at the position
  faab_pos$Pos_VORP_Adds[i] <- sum(faab_log$Added_Pos == faab_pos[i,1] & faab_log$Added_VORP_pts > 0 & faab_log$Success == "Yes" & !is.na(faab_log$Added_VORP_pts))
  faab_pos$Pos_VORP_PPG_Adds[i] <- sum(faab_log$Added_Pos == faab_pos[i,1] & faab_log$Added_VORP_ppg > 0 & faab_log$Success == "Yes" & !is.na(faab_log$Added_VORP_ppg))
  
  # Add the amount of VORP and VORP_ppg dropped by position
  faab_pos$VORP_Dropped[i] <- sum(faab_log$Dropped_VORP_pts[faab_log$Dropped_Pos == faab_pos[i,1] & faab_log$Dropped_VORP_pts > 0 & !is.na(faab_log$Dropped_VORP_pts)])
  faab_pos$VORP_PPG_Dropped[i] <- round(sum(faab_log$Dropped_VORP_ppg[faab_log$Dropped_Pos == faab_pos[i,1] & faab_log$Dropped_VORP_ppg > 0 & !is.na(faab_log$Dropped_VORP_ppg)]), digits = 2)
}
