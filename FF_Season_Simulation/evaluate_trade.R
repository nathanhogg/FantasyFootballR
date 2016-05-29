evaluate_trade <- function(side1, side2, roster_proj, league_size,
                           ff_team_lookup, ffl_schedule, sims, sim_sd) {
  
  source("process_trades.R")
  source("get_weekly_exp.R")
  source("simulate_season.R")

  team1_ID <- roster_proj$FF_Team_ID[roster_proj$Player == side1[1]]
  team2_ID <- roster_proj$FF_Team_ID[roster_proj$Player == side2[1]]
  
  team1_count <- length(side1)
  team2_count <- length(side2)
  
  roster_trade <- process_trades(side1, side2, roster_proj)
  
  # Get the weekly expected points for each team
  weekly_exp <- get_weekly_exp(roster_proj, league_size)
  weekly_exp <- merge(ff_team_lookup[ , c(1,3)], weekly_exp, by = "FF_Team_ID")
  
  weekly_trade <- get_weekly_exp(roster_trade, league_size)
  weekly_trade <- merge(ff_team_lookup[ , c(1,3)], weekly_trade, by = "FF_Team_ID")
  
  # Simulate each season
  summary <- simulate_season(weekly_exp, ffl_schedule, league_size, 
                             ff_team_lookup, sims, sim_sd)
  
  summary_trade <- simulate_season(weekly_trade, ffl_schedule, league_size, 
                                   ff_team_lookup, sims, sim_sd)
  
  difference <- data.frame(summary$Team, summary_trade[ , 2:5] - summary[ , 2:5])
    colnames(difference) <- c("Team", "Wins", "Points", "Standings", "Playoffs")
    
  difference <- merge(ff_team_lookup[ , c(1, 2)], difference, by.x = "Team_Name", by.y = "Team")
  
  if (team1_count == 1) {
    string1 <- side1[1]
  } else {
    if (team1_count == 2) {
      string1 <- paste0(side1[1], " and ", side1[2])
    } else {
      for (i in 1:team1_count) {
        if (i == 1) {
          string1 <- side1[1]
        } else {
          if (i == team1_count) {
            string1 <- paste0(string1, ", and ", side1[i])
          } else {
            string1 <- paste0(string1, ", ", side1[i])
          }
        }
      }
    }
  }
  
  if (team2_count == 1) {
    string2 <- side2[1]
  } else {
    if (team2_count == 2) {
      string2 <- paste0(side2[1], " and ", side2[2])
    } else {
      for (i in 1:team2_count) {
        if (i == 1) {
          string2 <- side2[1]
        } else {
          if (i == team2_count) {
            string2 <- paste0(string2, ", and ", side2[i])
          } else {
            string2 <- paste0(string2, ", ", side2[i])
          }
        }
      }
    }
  }
  
  wins1 <- format(difference$Wins[difference$FF_Team_ID == team1_ID], digits = 3)
  points1 <- format(difference$Points[difference$FF_Team_ID == team1_ID], digits = 4)
  odds1 <- format(difference$Playoffs[difference$FF_Team_ID == team1_ID], digits = 3)
  
  wins2 <- format(difference$Wins[difference$FF_Team_ID == team2_ID], digits = 3)
  points2 <- format(difference$Points[difference$FF_Team_ID == team2_ID], digits = 4)
  odds2 <- format(difference$Playoffs[difference$FF_Team_ID == team2_ID], digits = 3)
  
  cat(paste0("\n", "The results of trading ", string1, " for ", string2, 
             " across ", sims, " simulations are as follows:", "\n",
             "\n",
             "\t", difference$Team_Name[difference$FF_Team_ID == team1_ID], ":", "\n",
             "\t", "\t", "- change in wins:           ", ifelse(wins1 > 0, paste0("+", wins1), wins1), "\n",
             "\t", "\t", "- change in points:         ", ifelse(points1 > 0, paste0("+", points1), points1), "\n",
             "\t", "\t", "- change in playoff odds:   ", ifelse(odds1 > 0, paste0("+", odds1, "%"), paste0(odds1, "%")), "\n",
             "\n",
             "\t", difference$Team_Name[difference$FF_Team_ID == team2_ID], ":", "\n",
             "\t", "\t", "- change in wins:           ", ifelse(wins2 > 0, paste0("+", wins2), wins2), "\n",
             "\t", "\t", "- change in points:         ", ifelse(points2 > 0, paste0("+", points2), points2), "\n",
             "\t", "\t", "- change in playoff odds:   ", ifelse(odds2 > 0, paste0("+", odds2, "%"), paste0(odds2, "%")), "\n"))
  
  return(difference)
  
}
