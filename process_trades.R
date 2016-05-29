process_trades <- function(side1, side2, roster_proj) {
  
  team1_count <- length(side1)
  team2_count <- length(side2)
  
  team1_ID <- roster_proj$FF_Team_ID[roster_proj$Player == side1[1]]
  team2_ID <- roster_proj$FF_Team_ID[roster_proj$Player == side2[1]]
  
  for (i in 1:team1_count) {
    roster_proj$FF_Team_ID[roster_proj$Player == side1[i]] <- team2_ID
  }
  
  for (i in 1:team2_count) {
    roster_proj$FF_Team_ID[roster_proj$Player == side2[i]] <- team1_ID
  }
  
  return(roster_proj)
  
}