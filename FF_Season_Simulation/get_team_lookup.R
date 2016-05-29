get_team_lookup <- function() {
  
  team_abrv <- c("ARI", "SF", "SEA", "LA", "DAL", "PHI", "NYG", "WSH",
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
  
  team_lookup <- data.frame(team_names, team_abrv)
  colnames(team_lookup) <- c("Team_Name", "Team")
  
  return(team_lookup)
  
}
