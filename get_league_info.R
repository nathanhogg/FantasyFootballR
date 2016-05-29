get_league_info <- function(league_id, league_size) {
  
  library(rvest)
  library(tidyr)
  
  # Build id_table
  roster_url <- read_html(paste0("http://games.espn.go.com/ffl/leaguerosters?leagueId=", league_id))
  
  id_table <- data.frame(matrix(data = NA, nrow = league_size, ncol = 2))
  colnames(id_table) <- c("FF_Team_ID", "FF_Team")
  id_table$FF_Team_ID <- as.integer(1:league_size)
  
  for (i in 1:league_size) {
    
    roster <- html_table(html_nodes(roster_url, "table")[[i + 2]], fill = TRUE)
    id_table$FF_Team[i] <- names(roster)[1]
  
  }
  
  name_length <- min(nchar(id_table$FF_Team) - 6)
  id_table$FF_Team <- substr(id_table$FF_Team, 1, name_length)
  
  
  # Get division information
  url_divs <- read_html(paste0("http://games.espn.go.com/ffl/leaguesetup/settings?leagueId=", league_id))
  divisions <- html_table(html_nodes(url_divs, "table")[[12]], fill= TRUE, header = TRUE)
  
  divisions <- divisions[rowSums(!is.na(divisions)) > 2, ]
  divisions <- divisions[ , -2] 
  names(divisions) <- 0:(ncol(divisions) - 1)
  rownames(divisions) <- divisions[ , 1]
  divisions <- divisions[ , -1]
  divisions <- data.frame(t(divisions))
  divisions <- na.omit(divisions)
  
  n_div <- ncol(divisions)
  
  for (i in 1:n_div) {
    divisions[ , i] <- as.character(divisions[ , i])
  }
  
  # Get full names and short names
  ff_team_lookup <- gather(divisions)[2]
  colnames(ff_team_lookup) <- "Full_Name"
  
  ff_team_lookup$Short_Name <- toupper(substr(ff_team_lookup$Full_Name,
                               1, min(nchar(ff_team_lookup$Full_Name))))
  
  ff_team_lookup <- merge(ff_team_lookup, id_table, by.x = "Short_Name", by.y = "FF_Team")
  ff_team_lookup <- merge(ff_team_lookup, gather(divisions), by.x = "Full_Name", by.y = "value")
  
  colnames(ff_team_lookup) <- c("Team_Name", "Team_Abrv", "FF_Team_ID", "Division")
  ff_team_lookup <- ff_team_lookup[ , c(3, 1, 2, 4)]
  
  return(ff_team_lookup)
  
}