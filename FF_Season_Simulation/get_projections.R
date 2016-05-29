get_projections <- function () {
  
  library(rvest)
  library(stringr)
  
  # QB projections
  url_qb <- read_html("https://www.fantasypros.com/nfl/projections/qb.php?week=draft")
  qb_proj <- html_table(html_nodes(url_qb, "table")[[2]], fill = TRUE, header = TRUE)
  colnames(qb_proj) <- qb_proj[1, ]
  qb_proj <- qb_proj[-1, -c(2, 3, 4, 5, 6, 7, 8, 9, 10)]
  qb_proj$Pos <- "QB"
  qb_proj$Team <- lapply(qb_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  qb_proj$Player <- gsub("\\s*\\w*$", "", qb_proj$Player)
  qb_proj <- qb_proj[ , c(1, 4, 3, 2)]
  
  #RB Projections
  url_rb <- read_html("https://www.fantasypros.com/nfl/projections/rb.php?week=draft")
  rb_proj <- html_table(html_nodes(url_rb, "table")[[2]], fill = TRUE, header = TRUE)
  colnames(rb_proj) <- rb_proj[1, ]
  rb_proj <- rb_proj[-1, -c(2, 3, 4, 5, 6, 7, 8)]
  rb_proj$Pos <- "RB"
  rb_proj$Team <- lapply(rb_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  rb_proj$Player <- gsub("\\s*\\w*$", "", rb_proj$Player)
  rb_proj <- rb_proj[ , c(1, 4, 3, 2)]
  
  # WR Projections
  url_wr <- read_html("https://www.fantasypros.com/nfl/projections/wr.php?week=draft")
  wr_proj <- html_table(html_nodes(url_wr, "table")[[2]], fill = TRUE, header = TRUE)
  colnames(wr_proj) <- wr_proj[1, ]
  wr_proj <- wr_proj[-1, -c(2, 3, 4, 5, 6, 7, 8)]
  wr_proj$Pos <- "WR"
  wr_proj$Team <- lapply(wr_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  wr_proj$Player <- gsub("\\s*\\w*$", "", wr_proj$Player)
  wr_proj <- wr_proj[ , c(1, 4, 3, 2)]
  
  # TE Projections
  url_te <- read_html("https://www.fantasypros.com/nfl/projections/te.php?week=draft")
  te_proj <- html_table(html_nodes(url_te, "table")[[2]], fill = TRUE, header = TRUE)
  colnames(te_proj) <- te_proj[1, ]
  te_proj <- te_proj[-1, -c(2, 3, 4, 5)]
  te_proj$Pos <- "TE"
  te_proj$Team <- lapply(te_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  te_proj$Player <- gsub("\\s*\\w*$", "", te_proj$Player)
  te_proj <- te_proj[ , c(1, 4, 3, 2)]
  
  # D/ST Projections
  url_dst <- read_html("https://www.fantasypros.com/nfl/projections/dst.php?week=draft")
  dst_proj <- html_table(html_nodes(url_dst, "table")[[2]], fill = TRUE, header = TRUE)
  colnames(dst_proj) <- dst_proj[1, ]
  dst_proj <- dst_proj[-1, -c(2, 3, 4, 5, 6, 7, 8, 9, 10)]
  dst_proj$Pos <- "D/ST"
  dst_proj$Team <- lapply(dst_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  dst_proj$Player <- gsub("\\s*\\w*$", "", dst_proj$Player)
  dst_proj$Player <- paste0(word(dst_proj$Player, -1), " ", dst_proj$Pos)
  dst_proj <- dst_proj[ , c(1, 4, 3, 2)]
  
  # K Projections
  url_k <- read_html("https://www.fantasypros.com/nfl/projections/k.php?week=draft")
  k_proj <- html_table(html_nodes(url_k, "table")[[2]], fill = TRUE, header = TRUE)
  k_proj <- k_proj[ , -c(2, 3, 4)]
  k_proj$Pos <- "K"
  k_proj$Team <- lapply(k_proj$Player, function(x) tail(strsplit(x ,split=" ")[[1]], 1))
  k_proj$Player <- gsub("\\s*\\w*$", "", k_proj$Player)
  k_proj <- k_proj[ , c(1, 4, 3, 2)]
  
  projections <- rbind(qb_proj, rb_proj, wr_proj, te_proj, dst_proj, k_proj)
  
  # Set classes
  projections$Player <- as.character(projections$Player)
  projections$Team <- as.character(projections$Team)
  projections$Pos <- as.character(projections$Pos)
  projections$FPTS <- as.numeric(projections$FPTS)
  
  return(projections)
  
}
