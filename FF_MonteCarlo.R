rm(list = ls())

# Load the necessary packages for this program --------------------------------
library(rvest)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)
library(DT)

# Ask for constants that will change between runs -----------------------------
  # League info
  league_id <- 30173
    # 66943 = fish/aq redraft
    # 30173 = dynasty league
    # 255075 = HS league
    # 367938 = roommate league
  league_size <- 10
  n_playoff_teams <- 4
  
  # Simulation info
  sim_number <- 1000
  sim_sd <- 19.7
  
  # Number of each starter
  s_qb <- 1
  s_rb <- 2
  s_wr <- 2
  s_te <- 1
  s_fx <- 1
  s_ds <- 1
  s_k  <- 1
  
  # Value of a waiver wire player for each position
  w_qb <- 10
  w_rb <- 3
  w_wr <- 3
  w_te <- 3
  w_ds <- 5
  w_k  <- 6

# Get basic league information (teams, roster configurations, etc.) -----------
url_divs <- html(paste0("http://games.espn.go.com/ffl/leaguesetup/settings?",
                         "leagueId=", league_id))
divisions <- html_table(html_nodes(url_divs, "table")[[12]], fill= TRUE,
                        header = TRUE)
  divisions <- divisions[rowSums(!is.na(divisions)) > 2, ]
  divisions <- divisions[ , -2] 
  names(divisions) <- 0:(ncol(divisions) - 1)
  rownames(divisions) <- divisions[ , 1]
  divisions <- divisions[ , -1]
  divisions <- data.frame(t(divisions))
  divisions <- na.omit(divisions)
  n_div <- ncol(divisions)

FF_team_lookup <- gather(divisions)[2]
  names(FF_team_lookup) <- "Full_name"
  FF_team_lookup$Short_name <- toupper(substr(FF_team_lookup$Full_name,
                                              1, min(nchar(
                                                FF_team_lookup$Full_name))))

div_short <- data.frame(lapply(divisions, FUN = function(x) 
             toupper(substr(x, 1, min(nchar(FF_team_lookup$Full_name))))))
  names(div_short) <- rep("Team", n_div)

# Scrape ESPN league website for league schedule ------------------------------
url_schedule <- html(paste0("http://games.espn.go.com/ffl/schedule?leagueId=", 
                            league_id))
FFL_schedule <- html_table(html_nodes(url_schedule, "table")[[2]], fill = TRUE)
  colnames(FFL_schedule) <- FFL_schedule[2, ]
  FFL_schedule <- FFL_schedule[1:(nrow(FFL_schedule) - 6),]
  row_remove <- matrix(data = NA, nrow = 100, ncol = 1)
  row_remove[1] <- 1
  row_remove[2] <- 2
  for (i in 3:100) {
    if (i %% 3 == 0) {
      row_remove[i] <- row_remove[i - 1] + 1 + league_size / 2
    } else {
      row_remove[i] <- row_remove[i - 1] + 1 
    }
  }
  row_remove <- as.vector(row_remove)
  FFL_schedule <- FFL_schedule[-c(row_remove), -c(3)]
  names(FFL_schedule) <- c("Away_Team", "Away_Owner", "Home_Team", "Home_Owner",
                         "Result")
  weeks <- ceiling(1:nrow(FFL_schedule) / league_size * 2)
  FFL_schedule <- transform(FFL_schedule, Week = weeks)
  FFL_schedule <- FFL_schedule[ , c(6, 1, 2 ,3 ,4 ,5)]
  name_length <- min(nchar(FFL_schedule$Away_Team) - 6)
  FFL_schedule$Away_Team <- toupper(substr(FFL_schedule$Away_Team,
                                          1, name_length))
  FFL_schedule$Home_Team <- toupper(substr(FFL_schedule$Home_Team,
                                          1, name_length))

# Scrape NFL schedule data ----------------------------------------------------
url_nflschedule <- html("http://espn.go.com/nfl/schedulegrid")
  NFL_schedule <- html_table(html_nodes(url_nflschedule, "table")[[1]], 
                            fill = TRUE)
  colnames(NFL_schedule) <- NFL_schedule[2, ]
  NFL_schedule <- NFL_schedule[-c(1,2), ]
  NFL_schedule <- data.frame(lapply(NFL_schedule, 
                                    FUN = function(x) gsub("@", "", x)))
  NFL_schedule <- data.frame(lapply(NFL_schedule, 
                                    FUN = function(x) gsub("JAX", "JAC", x)))
  FA <- c("FA", rep("BYE", 17))
    FA <- t(FA)
    FA <- data.frame(FA)
    names(FA) <- names(NFL_schedule)
  NFL_schedule <- rbind(NFL_schedule, FA)
  rm(FA)

# Scrape ESPN website for Position Vs. Opponent data --------------------------
pos = c("QB", "RB", "WR", "TE", " ", " ", " ", " ",
        " ", " ", " ", " ", " ", " ", " ", "DST")

for (i in c(1, 2, 3, 4, 16)) {
  name <- paste0("PVO_",pos[i])
  url_pos <- html(paste0("http://games.espn.go.com/ffl/pointsagainst?",
                         "&positionId=", i))
  pos_table <- html_table(html_nodes(url_pos, "table")[[2]], fill = TRUE)
    pos_table <- pos_table[-(1:2), -(2:19)]
    colnames(pos_table) <- c("Team", "Avg")
    transform(pos_table[ , 2] <- as.numeric(pos_table[ , 2]))
    pos_table[paste0(pos[i], "ratio")] <- pos_table$Avg / mean(pos_table$Avg)
    pos_table <- pos_table[order(pos_table$Team), ]
    assign(name, pos_table)
    rm(pos_table)
}

PVO <- cbind.data.frame(PVO_QB$Team, PVO_QB$QBratio, PVO_RB$RBratio,
                        PVO_WR$WRratio, PVO_TE$TEratio, PVO_DST$DSTratio)
  colnames(PVO) <- c("Team", "QB", "RB", "WR", "TE", "DST")
  PVO$Team <- word(PVO$Team)
  rm(PVO_DST, PVO_QB, PVO_WR, PVO_RB, PVO_TE)
  
  team_abrv <- c("ARI", "SF", "SEA", "STL", "DAL", "PHI", "NYG", "WSH",
                "DET", "GB", "CHI", "MIN", "NO", "ATL", "TB", "CAR",
                "JAC", "HOU", "IND", "TEN", "NE", "MIA", "BUF", "NYJ",
                "DEN", "KC", "SD", "OAK", "BAL", "CIN", "CLE", "PIT")
  team_names <- c("Cardinals", "49ers", "Seahawks", "Rams",
                  "Cowboys", "Eagles", "Giants", "Redskins",
                  "Lions", "Packers", "Bears", "Vikings",
                  "Saints", "Falcons", "Buccaneers", "Panthers",
                  "Jaguars", "Texans", "Colts", "Titans",
                  "Patriots", "Dolphins", "Bills", "Jets",
                  "Broncos", "Chiefs", "Chargers", "Raiders",
                  "Ravens", "Bengals", "Browns", "Steelers")
  Team_lookup <- data.frame(team_names, team_abrv)
    colnames(Team_lookup) <- c("Team", "ABRV")
  PVO <- join(PVO, Team_lookup, by="Team")
  PVO <- PVO[ , c(7, 1, 2, 3, 4, 5, 6)]
  PVO_Bye <- list("BYE", "BYE", 0, 0, 0, 0, 0)
    PVO_Bye <- data.frame(PVO_Bye)
    names(PVO_Bye) <- names(PVO)
  PVO <- rbind(PVO, PVO_Bye)
  names(PVO[, 1]) <- "Opp"
  rm(PVO_Bye)

# Scrape player projection data -----------------------------------------------
url_espn_proj <- html("http://games.espn.go.com/ffl/tools/projections?")
Proj_espn <- html_table(html_nodes(url_espn_proj, "table")[[2]], fill = TRUE)
  for (i in 1:15) {
    url_string <- paste0("http://games.espn.go.com/ffl/tools/projections?",
                         "&startIndex=", i * 40)
    url_espn_proj_i <- html(url_string)
    Proj_espn_i <- html_table(html_nodes(url_espn_proj_i, "table")[[2]], 
                              fill = TRUE)
    Proj_espn_i <- Proj_espn_i[-1, ]
    Proj_espn <- rbind(Proj_espn, Proj_espn_i)
    rm(Proj_espn_i)
  }
  Proj_espn$TOTAL <- as.numeric(Proj_espn$TOTAL)
  names(Proj_espn) <- Proj_espn[1, ]
  Proj_espn <- Proj_espn[-1, ]
  Proj_espn <- separate(Proj_espn, 'PLAYER, TEAM POS', c("Player", "TeamPos"), 
                      sep = ", ", extra = "drop")
  Proj_espn$TeamPos <- gsub("\\s+", " ", Proj_espn$TeamPos)
  Proj_espn <- separate(Proj_espn, 'TeamPos', c("Team", "Pos"), 
                      sep = " ", extra = "drop")
  Proj_espn$Pos[is.na(Proj_espn$Pos)] <- "D/ST"
  Proj_espn$Player[is.na(Proj_espn$Team)] <- substr(Proj_espn$Player[
                                                    is.na(Proj_espn$Team)], 
                                                    1, nchar(Proj_espn$Player[
                                                      is.na(Proj_espn$Team)]) 
                                                    - 5)
  Proj_espn$Team[is.na(Proj_espn$Team)] <- substr(Proj_espn$Player[
                                                  is.na(Proj_espn$Team)], 
                                                  1, nchar(Proj_espn$Player[
                                                    is.na(Proj_espn$Team)]) 
                                                  - 5)
  Proj_espn <- merge(Proj_espn, Team_lookup, all.x = TRUE, 
                     by.x = "Team", by.y = "Team")
  Proj_espn$ABRV <- as.character(Proj_espn$ABRV)
  Proj_espn$RNK <- as.numeric(Proj_espn$RNK)
  Proj_espn$Team[nchar(Proj_espn$Team) > 3] <- Proj_espn$ABRV[
                                               !is.na(Proj_espn$ABRV)]
  Proj_espn$Team <- toupper(Proj_espn$Team)
  Proj_espn <- Proj_espn[ , c(2, 3, 1, 4, 5, 6, 7, 8, 9, 10, 
                              11, 12, 13, 14, 15, 16)]

# Build extra data frames and waiver wire players to be used in loop ----------
Weekly_expected <- data.frame(data =NA, nrow = league_size, ncol = 18)
  
roster_cols <- c("Team", "Player", "Position", "Projection", "Week1", 
                 "Week2", "Week3", "Week4", "Week5", "Week6", "Week7",
                 "Week8", "Week9", "Week10", "Week11", "Week12", 
                 "Week13", "Week14", "Week15", "Week16", "Week17")

waiver_qb <- data.frame("WVR", "Waiver QB", "QB", w_qb * 17, w_qb, w_qb, w_qb,
                        w_qb, w_qb, w_qb, w_qb, w_qb, w_qb, w_qb, w_qb, w_qb,
                        w_qb, w_qb, w_qb, w_qb, w_qb)
waiver_rb <- data.frame("WVR", "Waiver RB", "RB", w_rb * 17, w_rb, w_rb, w_rb,
                        w_rb, w_rb, w_rb, w_rb, w_rb, w_rb, w_rb, w_rb, w_rb,
                        w_rb, w_rb, w_rb, w_rb, w_rb)
waiver_wr <- data.frame("WVR", "Waiver WR", "WR", w_wr * 17, w_wr, w_wr, w_wr,
                        w_wr, w_wr, w_wr, w_wr, w_wr, w_wr, w_wr, w_wr, w_wr,
                        w_wr, w_wr, w_wr, w_wr, w_wr)
waiver_te <- data.frame("WVR", "Waiver TE", "TE", w_te * 17, w_te, w_te, w_te,
                        w_te, w_te, w_te, w_te, w_te, w_te, w_te, w_te, w_te,
                        w_te, w_te, w_te, w_te, w_te)
waiver_ds <- data.frame("WVR", "Waiver D/ST", "D/ST", w_ds * 17, w_ds, w_ds,
                        w_ds, w_ds, w_ds, w_ds, w_ds, w_ds, w_ds, w_ds, w_ds,
                        w_ds, w_ds, w_ds, w_ds, w_ds, w_ds)
waiver_k <- data.frame("WVR", "Waiver K", "K", w_k * 17, w_k, w_k, w_k, w_k,
                       w_k, w_k, w_k, w_k, w_k, w_k, w_k, w_k, w_k, w_k, w_k,
                       w_k, w_k)
names(waiver_qb) <- roster_cols  
names(waiver_rb) <- roster_cols 
names(waiver_wr) <- roster_cols 
names(waiver_te) <- roster_cols 
names(waiver_ds) <- roster_cols
names(waiver_k)  <- roster_cols

waivers <- data.frame(rbind(waiver_qb, waiver_rb, waiver_wr, waiver_te, 
                            waiver_ds, waiver_k))
rm(waiver_qb, waiver_rb, waiver_wr, waiver_te, waiver_ds, waiver_k)

# Scrape roster data from ESPN and fill out weekly expected lineup ------------
for (i in 3:(2 + league_size)) {
  url <- html(paste0("http://games.espn.go.com/ffl/leaguerosters?leagueId=", 
                     league_id))
  roster <- html_table(html_nodes(url, "table")[[i]], fill = TRUE)
    name <- names(roster)
    roster_name <- name[1]
    roster_name <- substr(roster_name, 1, nchar(roster_name) - 6)
    names(roster) <- roster[1, ]
    roster <- roster[-1, -c(1,3)]
    roster <- data.frame(roster)
    names(roster) <- "Player"
    roster$Player <- as.character(roster$Player)
    roster <- data.frame(roster$Player[nchar(roster$Player) > 5])
    names(roster) <- "Player"
    roster <- separate(roster, 'Player', c("Player", "TeamPos"), 
                       sep = ", ", extra = "drop")
    roster$TeamPos <- gsub("\\s+", " ", roster$TeamPos)
    roster <- separate(roster, 'TeamPos', c("Team", "Pos"), 
                       sep = " ", extra = "drop")
    roster$Pos[is.na(roster$Pos)] <- "D/ST"
    roster$Player[is.na(roster$Team)] <- substr(roster$Player[
                                                is.na(roster$Team)], 1, 
                                                nchar(roster$Player[
                                                is.na(roster$Team)]) - 5)
    roster$Team[is.na(roster$Team)] <- substr(roster$Player[
                                              is.na(roster$Team)], 1, 
                                              nchar(roster$Player[
                                              is.na(roster$Team)]) - 5)
    roster <- merge(roster, Team_lookup, all.x = TRUE, 
                  by.x = "Team", by.y = "Team")
    roster$ABRV <- as.character(roster$ABRV)
    roster$Team[nchar(roster$Team) > 3] <- roster$ABRV[!is.na(roster$ABRV)]
    roster$Team <- toupper(roster$Team)
    roster <- roster[ , c(2, 3, 1)]
  
  # Add projection data and NFL schedule to the roster
    roster <- merge(roster, Proj_espn[, c("Player", "NA")], all.x = TRUE)
    roster <- merge(roster, NFL_schedule, by.x = "Team", 
                    by.y = "TEAM", all.x = TRUE)
    names(roster[ , 4]) <- "Projection"
  
  # Convert the NFL schedule to PVO data for the roster 
    roster_qb <- roster[roster$Pos == "QB", ]
    roster_rb <- roster[roster$Pos == "RB", ]
    roster_wr <- roster[roster$Pos == "WR", ]
    roster_te <- roster[roster$Pos == "TE", ]
    roster_ds <- roster[roster$Pos == "D/ST", ]
  
  for (j in 5:21) {
    roster_qb <- merge(roster_qb, PVO[c("ABRV", "QB")], 
                       by.x = j, by.y = "ABRV", all.x = "TRUE")
    roster_rb <- merge(roster_rb, PVO[c("ABRV", "RB")], 
                       by.x = j, by.y = "ABRV", all.x = "TRUE")
    roster_wr <- merge(roster_wr, PVO[c("ABRV", "WR")], 
                       by.x = j, by.y = "ABRV", all.x = "TRUE")
    roster_te <- merge(roster_te, PVO[c("ABRV", "TE")], 
                       by.x = j, by.y = "ABRV", all.x = "TRUE")
    roster_ds <- merge(roster_ds, PVO[c("ABRV", "DST")], 
                       by.x = j, by.y = "ABRV", all.x = "TRUE")
  }
  
    roster_qb <- roster_qb[ , -c(1:17)]; names(roster_qb) <- roster_cols
    roster_rb <- roster_rb[ , -c(1:17)]; names(roster_rb) <- roster_cols
    roster_wr <- roster_wr[ , -c(1:17)]; names(roster_wr) <- roster_cols
    roster_te <- roster_te[ , -c(1:17)]; names(roster_te) <- roster_cols
    roster_ds <- roster_ds[ , -c(1:17)]; names(roster_ds) <- roster_cols
  
    roster <- rbind(roster_qb, roster_rb, roster_wr, roster_te, roster_ds)
    roster$Projection[is.na(roster$Projection)] <- 0
    rm(roster_qb, roster_rb, roster_wr, roster_te, roster_ds)
  
  # Multiply projections by PVO data to get weekly expected player points
    for (k in 5:21) {
      roster[ , k] <- roster[ , k] * roster$Projection / 17
    }
  
  # Add in default "waiver wire players" for each position
    roster <- data.frame(rbind(roster, waivers, waivers))
  
  # Pick the best lineup for each week and sum it up
    n_qb <- length(roster$Pos[roster$Pos == "QB"])
    n_rb <- length(roster$Pos[roster$Pos == "RB"])
    n_wr <- length(roster$Pos[roster$Pos == "WR"])
    n_te <- length(roster$Pos[roster$Pos == "TE"])
    n_ds <- length(roster$Pos[roster$Pos == "D/ST"])
    n_k  <- length(roster$Pos[roster$Pos == "K"])
    
    for (l in 5:21) {
      score <- 0
      
      # QB selection
      for (m in 1:s_qb) {
        if (m == 1) {
          score <- score + max(roster[ , l][roster$Pos == "QB"])
        } else {
          score <- score + sort(roster[ , l][roster$Pos == "QB"], 
                                partial = n_qb - m + 1)[n_qb - m + 1]
        }
      }
      
      # RB selection
      for (m in 1:s_rb) {
        if (m == 1) {
          score <- score + max(roster[ , l][roster$Pos == "RB"])
        } else {
          score <- score + sort(roster[ , l][roster$Pos == "RB"], 
                                partial = n_rb - m + 1)[n_rb - m + 1]
        }
      }
      
      # WR selection
      for (m in 1:s_wr) {
        if (m == 1) {
          score <- score + max(roster[ , l][roster$Pos == "WR"])
        } else {
          score <- score + sort(roster[ , l][roster$Pos == "WR"], 
                                partial = n_wr - m + 1)[n_wr - m + 1]
        }
      }
      
      # TE selection
      for (m in 1:s_te) {
        if (m == 1) {
          score <- score + max(roster[ , l][roster$Pos == "TE"])
        } else {
          score <- score + sort(roster[ , l][roster$Pos == "TE"], 
                                partial = n_te - m + 1)[n_te - m +1]
        }
      }
      
      FLEX <- c(sort(roster[ , l][roster$Pos == "RB"], 
                     partial = n_rb - s_rb)[n_rb - s_rb],
                sort(roster[ , l][roster$Pos == "RB"], 
                     partial = n_rb - s_rb - 1)[n_rb - s_rb - 1],
                sort(roster[ , l][roster$Pos == "WR"], 
                     partial = n_wr - s_wr)[n_wr - s_wr],
                sort(roster[ , l][roster$Pos == "WR"], 
                     partial = n_wr - s_wr - 1)[n_wr - s_wr - 1],
                sort(roster[ , l][roster$Pos == "TE"], 
                     partial = n_te - s_te)[n_te - s_te],
                sort(roster[ , l][roster$Pos == "TE"], 
                     partial = n_te - s_te - 1)[n_te - s_te - 1])
        n_fx <- length(FLEX)
      
      # Flex selection
      for (m in 1:s_fx) {
        if (m == 1) {
          score <- score + max(FLEX)
        } else {
          score <- score + sort(FLEX, partial = n_fx - m + 1)[n_fx - m + 1]
        }
      }
      
      # Defense selection
      for (m in 1:s_ds) {
        if (m == 1) {
          score <- score + max(roster[ , l][roster$Pos == "D/ST"])
        } else {
          score <- score + sort(roster[ , l][roster$Pos == "D/ST"], 
                                partial = n_ds - m + 1)[n_ds - m + 1]
        }
      }
      
      # Add in kicker if using kicker
      score <- score + s_k * w_k
      
      #qb_1 <- max(roster[ , l][roster$Pos == "QB"])
      #rb_1 <- max(roster[ , l][roster$Pos == "RB"])
      #rb_2 <- sort(roster[ , l][roster$Pos == "RB"], 
      #           partial = n_rb - 1)[n_rb - 1]
      #wr_1 <- max(roster[ , l][roster$Pos == "WR"])
      #wr_2 <- sort(roster[ , l][roster$Pos == "WR"], 
      #             partial = n_wr - 1)[n_wr - 1]
      #te_1 <- max(roster[ , l][roster$Pos == "TE"])
      #ds_1 <- max(roster[ , l][roster$Pos == "D/ST"])
      #fx_1 <- max(FLEX)
      #fx_2 <- sort(FLEX, partial = n_fx - 1)[n_fx - 1]
      
      Weekly_expected[i - 2, 1] <- substr(roster_name, 1, name_length)
        Weekly_expected[i - 2, l - 3] <- score
  }
  
  # Name the roster and continute looping
    assign(roster_name, roster)
    rm(roster)

}

        names(Weekly_expected) <- c("Team", 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                    10, 11, 12, 13, 14, 15, 16, 17)
        Weekly_lookup <- gather(Weekly_expected, "Week", "Score", 2:18)
        rm(waivers)

# Set up dataframes to be filled out during simulation ------------------------
  cum_wins <- data.frame(FF_team_lookup$Short_name)
    names(cum_wins) <- "Team"
  cum_playoffs <- data.frame(FF_team_lookup$Short_name)
    names(cum_playoffs) <- "Team"
  cum_points <- data.frame(FF_team_lookup$Short_name)
    names(cum_points) <- "Team"
  cum_stand <- data.frame(FF_team_lookup$Short_name)
    names(cum_stand) <- "Team"
    
# Simulate seasons ------------------------------------------------------------

for (i in 1:sim_number) {
  sim_scores <- lapply(Weekly_lookup$Score, FUN = function(x)
    rnorm(1, mean = x, sd = sim_sd))
    sim_scores <- data.frame(cbind(Weekly_lookup[ , 1], Weekly_lookup[ , 1],
                                    sim_scores))
    names(sim_scores) <- c("Away_Team", "Home_Team", "Score")
    sim_scores$Week <- ceiling(1:(17 * league_size) / league_size)
  
    sim_scores$Away_Team <- as.character(sim_scores$Away_Team)
    sim_scores$Home_Team <- as.character(sim_scores$Home_Team)
    sim_scores$Score <- as.numeric(sim_scores$Score)

  sim_schedule <- FFL_schedule
    sim_schedule <- merge(sim_schedule, sim_scores[ , c("Away_Team", 
                                                         "Score", "Week")],
                        by = c("Week", "Away_Team"),
                        all.x = TRUE)
    names(sim_schedule)[7] <- "Away_Score"
    sim_schedule <- merge(sim_schedule, sim_scores[ , c("Home_Team", 
                                                         "Score", "Week")],
                          by = c("Week", "Home_Team"),
                          all.x = TRUE)
    names(sim_schedule)[8] <- "Home_Score" 
    
    sim_schedule$Away_Score <- as.numeric(sim_schedule$Away_Score)
    sim_schedule$Home_Score <- as.numeric(sim_schedule$Home_Score)
    sim_schedule$winner <- ifelse(sim_schedule$Home_Score > 
                                  sim_schedule$Away_Score,
                                  sim_schedule$Home_Team, 
                                  sim_schedule$Away_Team)
  
  sim_wins <- data.frame(table(sim_schedule$winner))
    names(sim_wins) <- c("Team", paste0("W_sim",i))
    sim_wins <- merge(sim_wins, FF_team_lookup, by.y = "Short_name",
                  by.x = "Team", all.x = TRUE)
    sim_wins <- sim_wins[ , -3]
  cum_wins <- merge(cum_wins, sim_wins, all.x = TRUE)
  
  sim_points <- data.frame(cum_points[ 1])
    sim_points <- sim_scores[, 2:3]
    sim_points <- spread(sim_points, Home_Team, Score, drop = TRUE)
    sim_points[is.na(sim_points)] <- 0
    sim_points <- sim_points[1:(league_size * 13), ]
    sim_points <- data.frame(names(sim_points), colSums(sim_points))
    row.names(sim_points) <- 1:league_size
    names(sim_points) <- c("Team", paste0("P_sim", i))
  cum_points <- merge(cum_points, sim_points, all.x = TRUE)
  
  for (j in 1:n_div) {
    sim_standings <- merge(div_short[j], sim_wins, all.x = TRUE)
      sim_standings <- merge(sim_standings, sim_points, all.x = TRUE)
      sim_standings$Ranker <- sim_standings[2] + sim_standings[3] / 10000
      sim_standings$Playoffs <- ifelse(rank(sim_standings[,4]) > league_size/2 
                                       - n_playoff_teams/n_div, 1, 0)
      
      #if (j == 1) {
      #  sim_playoffs <- sim_standings[sim_standings$Playoffs == 1]
      #} else {
      #  
      #}
      
      names(sim_standings) <- c("Team", paste0("W_sim", i), paste0("P_sim", i),
                              paste0("R_sim", i), paste0("PO_sim", i, "_", j))
    cum_playoffs <- merge(cum_playoffs, sim_standings[ , c(1, 5)],
                          all.x = TRUE)
      div_name <- paste0("div_", j, "standings")
      assign(div_name, sim_standings)
      
      cum_playoffs[is.na(cum_playoffs)] <- 0
  }
  
  sim_finish <- merge(sim_wins, sim_points, all.x=TRUE)
    sim_finish$Total <- sim_finish[2] + sim_finish[3] / 10000
    sim_finish$Final <- league_size - rank(sim_finish[,4]) + 1
    names(sim_finish) <- c("Team", paste0("W_sim", i), paste0("P_sim", i),
                              paste0("R_sim", i), paste0("F_sim", i))
    cum_stand <- merge(cum_stand, sim_finish[ , c(1, 5)], all.x = TRUE)
    cum_stand[is.na(cum_stand)] <- 0
  
  if (i %% 100 == 0) {
        print(paste0(i/sim_number*100,"%"))
      }
}
  
  cum_wins[is.na(cum_wins)] <- 0

# Present data ----------------------------------------------------------------
    
  Summary <- data.frame(cum_wins[ , 1])
    names(Summary) <- "Team"
    Summary$Avg_wins <- rowSums(cum_wins[2:ncol(cum_wins)]) / i
    Summary$Avg_points <- rowSums(cum_points[2:ncol(cum_points)]) / i
    Summary$Playoff_Odds <- rowSums(cum_playoffs[2:ncol(cum_playoffs)])/i*100
    
    Summary <- merge(FF_team_lookup, Summary, by.x = "Short_name", by.y = "Team")
    Summary <- Summary[ , -1]
    names(Summary) <- c("Team", "Expected Wins", 
                        "Expected Points", "Playoff Odds (%)")
    Summary$`Expected Points` <- as.numeric(format(Summary$`Expected Points`,
                                                   digits = 5))
  datatable(Summary)
  
  for (i in 1:league_size) {
    hist(as.numeric(cum_stand[i, 2:sim_number]), main = paste0(
      "Expected place in final standings: ",as.character(cum_stand[i,1])))
  }
  
