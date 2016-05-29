simulate_season <- function(weekly_exp, ffl_schedule, league_size, 
                            ff_team_lookup, sims = 1000, sim_sd = 20) {
  
  # Create empty data frames that fill up as the simulation progresses
  wins <- cbind(ff_team_lookup$Team_Abrv, data.frame(matrix(data = 0, nrow = league_size, ncol = sims)))
    colnames(wins) <- c("Team_Abrv", 1:sims)
  
  points <- cbind(ff_team_lookup$Team_Abrv, data.frame(matrix(data = 0, nrow = league_size, ncol = sims)))
    colnames(points) <- c("Team_Abrv", 1:sims)
  
  standings <- cbind(ff_team_lookup$Team_Abrv, data.frame(matrix(data = 0, nrow = league_size, ncol = sims)))
    colnames(standings) <- c("Team_Abrv", 1:sims)
  
  # Simulate season in a for loop
  for (i in 1:sims) {
    
    # Create scores for the iteration using the weekly expected and the sim_sd
    sim_scores <- weekly_exp
    variation <- matrix(rnorm(17*league_size, mean = 0, sd = sim_sd), nrow = league_size, ncol = 17)
    sim_scores[ ,3:19] <- sim_scores[ ,3:19] + variation
    
    # Use the ffl_schedule for the simulation
    sim_schedule <- ffl_schedule
    sim_schedule$Home_Score <- 0
    sim_schedule$Away_Score <- 0
    
    # Fill in the scores of each game in the schedule and determine a winner
    for (j in 1:nrow(sim_schedule)) {
      sim_schedule$Home_Score[j] <- sim_scores[sim_scores$Team_Abrv == sim_schedule$Home_Abbr[j], sim_schedule$Week[j] + 2]
      sim_schedule$Away_Score[j] <- sim_scores[sim_scores$Team_Abrv == sim_schedule$Away_Abbr[j], sim_schedule$Week[j] + 2]
      
      if (sim_schedule$Home_Score[j] > sim_schedule$Away_Score[j]) {
        sim_schedule$Winner[j] <- sim_schedule$Home_Abbr[j]
      } else {
        sim_schedule$Winner[j] <- sim_schedule$Away_Abbr[j]
      }
    }
    
    # Count the wins and sum the points for this iteration of the simulation
    for (j in 1:league_size) {
      wins[j, i + 1] <- length(grep(wins$Team_Abrv[j], sim_schedule$Winner))
      points[j, i + 1] <- sum(sim_scores[sim_scores$Team_Abrv == points$Team_Abrv[j], 3:15])
    }
    
    # Update on progress of the for loop
    if (i %% 100 == 0) {
      print(paste0(i/sims*100,"%"))
    }
    
  }
  
  # Compute a metric that will be used for ranking teams based on wins and points
  win_points <- data.frame(wins$Team_Abrv, wins[ ,2:(sims + 1)] + points[ ,2:(sims + 1)]/10000)
  colnames(win_points) <- colnames(wins)
  
  # Calculate standings within the whole league
  standings <- data.frame(win_points$Team_Abrv, apply(-win_points[ , 2:(sims + 1)], 2, rank))
  colnames(standings) <- colnames(win_points)
  
  # Calculate standings within each division (for playoffs)
  div_win_points <- merge(ff_team_lookup[ , c(3, 4)], win_points, by = "Team_Abrv")
  divs <- unique(div_win_points$Division)
  
  for (i in 1:length(divs)) {
    standings_i <- div_win_points[div_win_points$Division == divs[i], ]
    temp <- data.frame(standings_i[ , c(1, 2)], apply(-standings_i[ , 3:(sims + 2)], 2, rank))
    if (i == 1) {
      div_standings <- temp
    } else {
      div_standings <- rbind(div_standings, temp)
    }
  }
  
  # Determine who makes the playoffs
  playoffs <- data.frame(div_standings$Team_Abrv, ifelse(div_standings[ , 3:(sims + 2)] < 3, 1, 0))
  colnames(playoffs) <- colnames(wins)
  
  # Compute averages
  win_avg <- data.frame(wins$Team_Abrv, rowMeans(wins[2:(sims+1)]))
  colnames(win_avg) <- c("Team_Abrv", "Avg_Wins")
  win_avg$Avg_Wins <- as.numeric(format(win_avg$Avg_Wins, digits = 3))
  
  points_avg <- data.frame(points$Team_Abrv, rowMeans(points[2:(sims+1)]))
  colnames(points_avg) <- c("Team_Abrv", "Avg_Points")
  points_avg$Avg_Points <- as.numeric(format(points_avg$Avg_Points, digits = 5))
  
  standings_avg <- data.frame(standings$Team_Abrv, rowMeans(standings[2:(sims+1)]))
  colnames(standings_avg) <- c("Team_Abrv", "Avg_Standings")
  standings_avg$Avg_Standings <- as.numeric(format(standings_avg$Avg_Standings, digits = 3))
  
  playoff_odds <- data.frame(playoffs$Team_Abrv, rowSums(playoffs[2:(sims+1)])*100/sims)
  colnames(playoff_odds) <- c("Team_Abrv", "Playoff_Odds")
  
  # Summarize the data and output the summary
  Summary <- merge(win_avg, points_avg, by = "Team_Abrv")
  Summary <- merge(Summary, standings_avg, by = "Team_Abrv")
  Summary <- merge(Summary, playoff_odds, by = "Team_Abrv")
  Summary <- merge(Summary, ff_team_lookup[ , c(2, 3)], by = "Team_Abrv")
  
  Summary <- Summary[ , c(6, 2, 3, 4, 5)]
  colnames(Summary) <- c("Team", "Avg Wins", "Avg Points", "Avg Standings", "Playoff Odds (%)")
  
  return(Summary)
  
}