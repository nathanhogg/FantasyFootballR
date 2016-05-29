get_nfl_schedule <- function() {
  
  library(rvest)
  
  url_nfl_schedule <- read_html("http://espn.go.com/nfl/schedulegrid")
  nfl_schedule <- html_table(html_nodes(url_nfl_schedule, "table")[[1]], fill = TRUE)
  
  nfl_schedule <- data.frame(lapply(nfl_schedule, 
                                    FUN = function(x) gsub("@", "", x)))
  nfl_schedule <- data.frame(lapply(nfl_schedule, 
                                    FUN = function(x) gsub("JAX", "JAC", x)))
  nfl_schedule <- data.frame(lapply(nfl_schedule, as.character))
  
  colnames(nfl_schedule) <- unlist(nfl_schedule[2, ], use.names = FALSE)
  nfl_schedule <- nfl_schedule[-c(1,2), ]
  
  FA <- data.frame(t(c("FA", rep("BYE", 17))))
  
  names(FA) <- names(nfl_schedule)
  nfl_schedule <- rbind(nfl_schedule, FA)
  rm(FA)
  
  return(nfl_schedule)
  
}
