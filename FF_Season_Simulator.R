# Script Name:  FF_Season_Simulator.R
#      Author:  Nathan Hogg
#  Orig. Date:  5/27/2016
#    Overview:  This script seeks to simulate an ESPN fantasy football season
#               using projections from around the web, as well as to evaluate
#               the effect of trades within said league

# Clear the global environment
rm(list = ls())

# Load all necessary functions for simulation
source("get_league_info.R")
source("get_projections.R")
source("get_espn_proj.R")
source("get_nfl_schedule.R")
source("get_ffl_schedule.R")
source("get_team_lookup.R")
source("get_pvo.R")
source("get_ff_rosters.R")
source("roster_proj.R")
source("process_trades.R")
source("evaluate_trade.R")
source("get_weekly_exp.R")
source("simulate_season.R")

# Set simulation parameters
league_id <- "30173"      # 30173 (Dynasty), 
league_size <- 10
sims <- 2500
sim_sd <- 20
proj_source <- "ESPN"     # Options are "ESPN" or "fantasypros.com consensus"

# Is this going to be a trade evaluation simulation?
trade <- FALSE
  side1 <- c("Allen Hurns", "Jay Ajayi")
  side2 <- "Adrian Peterson"

# Get NFL information
nfl_schedule <- get_nfl_schedule()
team_lookup <- get_team_lookup()
pvo <- get_pvo()

# Get FFL information
ff_team_lookup <- get_league_info(league_id, league_size)
ffl_schedule <- get_ffl_schedule(league_id, league_size)
all_rosters <- get_ff_rosters(league_id, league_size)

# Get fantasy player projections
if (proj_source == "fantasypros.com consensus") {
  projections <- get_projections()
  projections <- merge(projections, team_lookup, by = "Team")
  projections <- projections[ , c(1:4)]
} else if (proj_source == "ESPN") {
  projections <- get_espn_proj()
}

# Merge the projections with the ffl rosters
roster_proj <- roster_proj(all_rosters, nfl_schedule, pvo, projections)

# Run different code depending on if this is a trade evaluation
if (trade == TRUE) {
  difference <- evaluate_trade(side1, side2, roster_proj, league_size,
                               ff_team_lookup, ffl_schedule, sims, sim_sd)
} else {
  # Get the weekly expected points for each team
  weekly_exp <- get_weekly_exp(roster_proj, league_size)
  weekly_exp <- merge(ff_team_lookup[ , c(1,3)], weekly_exp, by = "FF_Team_ID")
  
  # Simulate the season
  summary <- simulate_season(weekly_exp, ffl_schedule, league_size, 
                             ff_team_lookup, sims, sim_sd)
  
  # Output results
  print(paste0("The season was simulated ", sims, " times using ", proj_source, 
    " projections with the following results:"))
  print(summary)
}
