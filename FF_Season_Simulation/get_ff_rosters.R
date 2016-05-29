get_ff_rosters <- function(league_id, league_size) {
  
  library(rvest)
  library(tidyr)
  
  roster_url <- read_html(paste0("http://games.espn.go.com/ffl/leaguerosters?leagueId=", league_id))

  for (i in 1:league_size) {
    
    roster <- html_table(html_nodes(roster_url, "table")[[i + 2]], fill = TRUE)
    colnames(roster) <- roster[1, ]
    roster <- roster[-c(1), -c(3)]
    roster$FF_Team_ID <- i
    
    if (i == 1) {
      all_rosters <- roster
    } else {
      all_rosters <- rbind(all_rosters, roster)
    }
  }
  
  colnames(all_rosters) <- c("Slot", "Player_Team_Pos", "FF_Team_ID")
  
  all_rosters <- separate(all_rosters, "Player_Team_Pos", c("Player", "TeamPos"), sep = ", ", extra = "drop")
  all_rosters$TeamPos <- gsub("\\s+", " ", all_rosters$TeamPos)
  all_rosters <- separate(all_rosters, "TeamPos", c("Team", "Pos"), sep = " ", extra = "drop")
  
  all_rosters <- all_rosters[ , c(2:5)]
  all_rosters$Team <- toupper(all_rosters$Team)
  all_rosters <- data.frame(lapply(all_rosters, FUN = function(x) gsub("JAX", "JAC", x)))
  
  all_rosters$Player <- as.character(all_rosters$Player)
  all_rosters$Team <- as.character(all_rosters$Team)
  all_rosters$Pos <- as.character(all_rosters$Pos)
  
  all_rosters$FF_Team_ID <- as.integer(as.character(all_rosters$FF_Team_ID))
  
  all_rosters$Pos[is.na(all_rosters$Pos)] <- "D/ST"
  all_rosters$Player[is.na(all_rosters$Team)] <- substr(all_rosters$Player[is.na(all_rosters$Team)], 1, nchar(all_rosters$Player[is.na(all_rosters$Team)]) - 5)
  all_rosters$Team[is.na(all_rosters$Team)] <- substr(all_rosters$Player[is.na(all_rosters$Team)], 1, nchar(all_rosters$Player[is.na(all_rosters$Team)]) - 5)

  test <- merge(all_rosters, team_lookup, all.x = TRUE, by.x = "Team", by.y = "Team")
  test$Team_Name <- as.character(test$Team_Name)
  replacements <- test$Team[is.na(test$Team_Name)]
  test$Team_Name[is.na(test$Team_Name)] <- replacements
  colnames(test) <- c("Team1", "Player", "Pos", "FF_Team_ID", "Team_Name")
  
  test2 <- merge(test, team_lookup, by = "Team_Name")
  test2$Team <- as.character(test2$Team)
  all_rosters <- test2[ , c(3, 6, 4, 5)]
  
  return(all_rosters)
  
}
