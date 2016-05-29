get_ffl_schedule <- function(league_id, league_size) {
  
  library(rvest)
  
  url_schedule <- read_html(paste0("http://games.espn.go.com/ffl/schedule?leagueId=", league_id))
  ffl_schedule <- html_table(html_nodes(url_schedule, "table")[[2]], fill = TRUE)
  
  colnames(ffl_schedule) <- ffl_schedule[2, ]
  ffl_schedule <- ffl_schedule[1:(nrow(ffl_schedule) - 6),]
  
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
  
  ffl_schedule <- ffl_schedule[-c(row_remove), -c(3)]
  names(ffl_schedule) <- c("Away_Team", "Away_Owner", "Home_Team", "Home_Owner", "Result")
  
  weeks <- ceiling(1:nrow(ffl_schedule) / league_size * 2)
  ffl_schedule <- transform(ffl_schedule, Week = weeks)
  
  name_length <- min(nchar(ffl_schedule$Away_Team) - 6)
  ffl_schedule$Away_Abbr <- toupper(substr(ffl_schedule$Away_Team, 1, name_length))
  ffl_schedule$Home_Abbr <- toupper(substr(ffl_schedule$Home_Team, 1, name_length))

  ffl_schedule <- ffl_schedule[ , c(6, 1, 7, 2, 3, 8, 4 ,5)]
  
  return(ffl_schedule)
  
}
