roster_proj <- function(all_rosters, nfl_schedule, pvo, projections) {
  
  # Merge rosters with projections and schedule
  all_rosters <- merge(all_rosters, projections[ , c(2, 4)], all.x = TRUE, by = "Player")
  all_rosters$FPTS[is.na(all_rosters$FPTS)] <- 0
  all_rosters <- merge(all_rosters, nfl_schedule, all.x = TRUE, by.x = "Team", by.y = "TEAM")
  
  # Convert the NFL schedule to PVO data for the roster 
  all_qb <- all_rosters[all_rosters$Pos == "QB", ]
  all_rb <- all_rosters[all_rosters$Pos == "RB", ]
  all_wr <- all_rosters[all_rosters$Pos == "WR", ]
  all_te <- all_rosters[all_rosters$Pos == "TE", ]
  all_k  <- all_rosters[all_rosters$Pos == "K", ]
  all_ds <- all_rosters[all_rosters$Pos == "D/ST", ]
  
  # Merge each positional data frame with the pvo for that position for each week
  for (j in 6:22) {
    all_qb <- merge(all_qb, pvo[c("Team", "QB")], by.x = j, by.y = "Team", all.x = "TRUE")
    all_rb <- merge(all_rb, pvo[c("Team", "RB")], by.x = j, by.y = "Team", all.x = "TRUE")
    all_wr <- merge(all_wr, pvo[c("Team", "WR")], by.x = j, by.y = "Team", all.x = "TRUE")
    all_te <- merge(all_te, pvo[c("Team", "TE")], by.x = j, by.y = "Team", all.x = "TRUE")
    all_k  <- merge(all_k,  pvo[c("Team", "K")],  by.x = j, by.y = "Team", all.x = "TRUE")
    all_ds <- merge(all_ds, pvo[c("Team", "DST")], by.x = j, by.y = "Team", all.x = "TRUE")
  }
  
  # Prepare column names for each positional data frame
  roster_cols <- c("Team", "Player", "Position", "FF_Team_ID", "FPTS", "Week1", 
                   "Week2", "Week3", "Week4", "Week5", "Week6", "Week7",
                   "Week8", "Week9", "Week10", "Week11", "Week12", 
                   "Week13", "Week14", "Week15", "Week16", "Week17")
  
  # For each position, remove unwanted columns and give consistent column names
  all_qb <- all_qb[ , -c(1:17)]; colnames(all_qb) <- roster_cols
  all_rb <- all_rb[ , -c(1:17)]; colnames(all_rb) <- roster_cols
  all_wr <- all_wr[ , -c(1:17)]; colnames(all_wr) <- roster_cols
  all_te <- all_te[ , -c(1:17)]; colnames(all_te) <- roster_cols
  all_k  <- all_k[ , -c(1:17)];  colnames(all_k) <- roster_cols
  all_ds <- all_ds[ , -c(1:17)]; colnames(all_ds) <- roster_cols
  
  # Recombine the positions back into a single table
  roster_proj <- rbind(all_qb, all_rb, all_wr, all_te, all_k, all_ds)
  
  # Multiply projections by PVO data to get weekly expected player points
  for (k in 6:22) {
    
    roster_proj[ , k] <- roster_proj[ , k] * roster_proj$FPTS / 16
  
  }
  
  # Re-order the columns so "Player" is first
  roster_proj <- roster_proj[ , c(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                  13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]
  
  # Output the roster_projections
  return(roster_proj)
  
}