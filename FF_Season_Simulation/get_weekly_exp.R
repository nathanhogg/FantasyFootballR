get_weekly_exp <- function(roster_proj, league_size, 
                           w_qb = 10, w_rb = 3, w_wr = 3, w_te = 3, w_k = 5, w_ds = 5,
                           s_qb = 1, s_rb = 2, s_wr = 2, s_te = 1, s_fx = 2, s_k = 0, s_ds = 1) {
  
  # Create empty dataframe for weekly totals to be input
  weekly_expected <- data.frame(matrix(0, nrow = league_size, ncol = 18))
  colnames(weekly_expected) <- c("FF_Team_ID", "Wk1", "Wk2", "Wk3", "Wk4",
                                 "Wk5", "Wk6", "Wk7", "Wk8", "Wk9", "Wk10", 
                                 "Wk11", "Wk12", "Wk13", "Wk14", "Wk15",
                                 "Wk16", "Wk17")
  weekly_expected$FF_Team_ID <- as.integer(1:league_size)

  # Create waiver wire players for each roster
  waiver_qb <- data.frame("Waiver QB", "WVR", "QB", league_size + 1, w_qb * 16, t(rep(w_qb, 17)))
  waiver_rb <- data.frame("Waiver RB", "WVR", "RB", league_size + 1, w_rb * 16, t(rep(w_rb, 17)))
  waiver_wr <- data.frame("Waiver WR", "WVR", "WR", league_size + 1, w_wr * 16, t(rep(w_wr, 17)))
  waiver_te <- data.frame("Waiver TE", "WVR", "TE", league_size + 1, w_te * 16, t(rep(w_te, 17)))
  waiver_k  <- data.frame("Waiver K", "WVR", "K", league_size + 1, w_k * 16, t(rep(w_k, 17)))
  waiver_ds <- data.frame("Waiver D/ST", "WVR", "D/ST", league_size + 1, w_ds * 16, t(rep(w_ds, 17)))
  
  colnames(waiver_qb) <- colnames(roster_proj) 
  colnames(waiver_rb) <- colnames(roster_proj)  
  colnames(waiver_wr) <- colnames(roster_proj)  
  colnames(waiver_te) <- colnames(roster_proj)  
  colnames(waiver_ds) <- colnames(roster_proj) 
  colnames(waiver_k)  <- colnames(roster_proj)
  
  waivers <- rbind(waiver_qb, waiver_qb, waiver_rb, waiver_rb, waiver_rb, waiver_rb,
                   waiver_wr, waiver_wr, waiver_wr, waiver_wr, waiver_te, waiver_te,
                   waiver_k, waiver_k, waiver_ds, waiver_ds)
  
  # Select the best lineup for each team for each week
  for (i in 1:league_size) {
    
    # Create current team roster and merge with waivers dataframe
    roster <- rbind(roster_proj[roster_proj$FF_Team_ID == i, ], waivers)
    
    n_qb <- length(roster$Pos[roster$Pos == "QB"])
    n_rb <- length(roster$Pos[roster$Pos == "RB"])
    n_wr <- length(roster$Pos[roster$Pos == "WR"])
    n_te <- length(roster$Pos[roster$Pos == "TE"])
    n_ds <- length(roster$Pos[roster$Pos == "D/ST"])
    n_k  <- length(roster$Pos[roster$Pos == "K"])
    
    for (j in 6:22) {
      
      score <- 0
      
      # QB selection
      for (k in 1:s_qb) {
        if (k == 1) {
          score <- score + max(roster[ , j][roster$Pos == "QB"])
        } else {
          score <- score + sort(roster[ , j][roster$Pos == "QB"], partial = n_qb - k + 1)[n_qb - k + 1]
        }
      }
      
      # RB selection
      for (k in 1:s_rb) {
        if (k == 1) {
          score <- score + max(roster[ , j][roster$Pos == "RB"])
        } else {
          score <- score + sort(roster[ , j][roster$Pos == "RB"], partial = n_rb - k + 1)[n_rb - k + 1]
        }
      }
      
      # WR selection
      for (k in 1:s_wr) {
        if (k == 1) {
          score <- score + max(roster[ , j][roster$Pos == "WR"])
        } else {
          score <- score + sort(roster[ , j][roster$Pos == "WR"], partial = n_wr - k + 1)[n_wr - k + 1]
        }
      }
      
      # TE selection
      for (k in 1:s_te) {
        if (k == 1) {
          score <- score + max(roster[ , j][roster$Pos == "TE"])
        } else {
          score <- score + sort(roster[ , j][roster$Pos == "TE"], partial = n_te - k + 1)[n_te - k + 1]
        }
      }
      
      # K selection
      if (s_k > 0) {
        for (k in 1:s_k) {
          if (k == 1) {
            score <- score + max(roster[ , j][roster$Pos == "K"])
          } else {
            score <- score + sort(roster[ , j][roster$Pos == "K"], partial = n_k - k + 1)[n_k - k + 1]
          }
        }
      }  
      
      # D/ST selection
      if (s_ds > 0) {
        for (k in 1:s_ds) {
          if (k == 1) {
            score <- score + max(roster[ , j][roster$Pos == "D/ST"])
          } else {
            score <- score + sort(roster[ , j][roster$Pos == "D/ST"], partial = n_ds - k + 1)[n_ds - k + 1]
          }
        }
      }
      
      # Flex selection
      flex <- c(sort(roster[ , j][roster$Pos == "RB"], partial = n_rb - s_rb)[n_rb - s_rb],
                sort(roster[ , j][roster$Pos == "RB"], partial = n_rb - s_rb - 1)[n_rb - s_rb - 1],
                sort(roster[ , j][roster$Pos == "RB"], partial = n_rb - s_rb - 2)[n_rb - s_rb - 2],
                sort(roster[ , j][roster$Pos == "WR"], partial = n_wr - s_wr)[n_wr - s_wr],
                sort(roster[ , j][roster$Pos == "WR"], partial = n_wr - s_wr - 1)[n_wr - s_wr - 1],
                sort(roster[ , j][roster$Pos == "WR"], partial = n_wr - s_wr - 2)[n_wr - s_wr - 2],
                sort(roster[ , j][roster$Pos == "TE"], partial = n_te - s_te)[n_te - s_te],
                sort(roster[ , j][roster$Pos == "TE"], partial = n_te - s_te - 1)[n_te - s_te - 1],
                sort(roster[ , j][roster$Pos == "TE"], partial = n_te - s_te - 2)[n_te - s_te - 2])
      n_fx <- length(flex)
      
      if (s_fx > 0) {
        for (k in 1:s_fx) {
          if (k == 1) {
            score <- score + max(flex)
          } else {
            score <- score + sort(flex, partial = n_fx - k + 1)[n_fx - k + 1]
          }
        }
      }
    
      weekly_expected[i, j - 4] <- score
      
    }
  }
  
  return(weekly_expected)
  
}
