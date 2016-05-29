get_espn_proj <- function() {
  
  library(rvest)
  library(tidyr)
  
  source("get_team_lookup.R")
  
  url_espn_proj <- read_html("http://games.espn.go.com/ffl/tools/projections?")
  espn_proj <- html_table(html_nodes(url_espn_proj, "table")[[2]], fill = TRUE)
  
  for (i in 1:15) {
    
    url_i <- read_html(paste0(
      "http://games.espn.go.com/ffl/tools/projections?&startIndex=", i * 40))

    espn_proj_i <- html_table(html_nodes(url_i, "table")[[2]], fill = TRUE)
      espn_proj_i <- espn_proj_i[-1, ]
      espn_proj <- rbind(espn_proj, espn_proj_i)
      rm(espn_proj_i)
    
  }
  
  espn_proj$TOTAL <- as.numeric(espn_proj$TOTAL)
  names(espn_proj) <- espn_proj[1, ]
  espn_proj <- espn_proj[-1, ]
  
  # Create new columns for player, team, and position
  espn_proj <- separate(espn_proj, 'PLAYER, TEAM POS', c("Player", "TeamPos"), sep = ", ", extra = "drop")
  espn_proj$TeamPos <- gsub("\\s+", " ", espn_proj$TeamPos)
  espn_proj <- separate(espn_proj, 'TeamPos', c("Team", "Pos"), sep = " ", extra = "drop")
  espn_proj$Team <- toupper(espn_proj$Team)
  espn_proj$Team <- gsub("JAX", "JAC", espn_proj$Team)
  
  # Fix the Defense columns
  espn_proj$Pos[is.na(espn_proj$Pos)] <- "D/ST"
  espn_proj$Player[is.na(espn_proj$Team)] <- substr(espn_proj$Player[is.na(espn_proj$Team)], 1, nchar(espn_proj$Player[is.na(espn_proj$Team)]) - 5)
  espn_proj$Team[is.na(espn_proj$Team)]   <- substr(espn_proj$Player[is.na(espn_proj$Team)], 1, nchar(espn_proj$Player[is.na(espn_proj$Team)]) - 5)
  espn_proj <- espn_proj[ , c(2, 3, 4, 15)]
  colnames(espn_proj) <- c("Player", "Team", "Pos", "FPTS")
  team_lookup <- get_team_lookup()
  test <- merge(espn_proj, team_lookup, all.x = TRUE, by = "Team")
  test$Team_Name <- as.character(test$Team_Name)
  replacements <- test$Team[is.na(test$Team_Name)]
  test$Team_Name[is.na(test$Team_Name)] <- replacements
  colnames(test) <- c("Team1", "Player", "Pos", "FPTS", "Team_Name")
  test2 <- merge(test, team_lookup, by = "Team_Name")
  test2$Team <- as.character(test2$Team)
  
  espn_proj <- test2[ , c(6, 3, 4, 5)]
  
  return(espn_proj)

}
