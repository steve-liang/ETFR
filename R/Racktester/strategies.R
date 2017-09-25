# Strategies takes two xts as inputs, the secondary one can be empty if the strategy is to be ran on primary series only
# Output format is in 4 columns xts with Close/Ema/Momentum/Sig/Pos or Close/Ema/Zscore/Sig/Pos in FULL Length as the original primary series

MomentumStrat <- function(primary, secondary, ema_smoothing, mom_period) {
  #This is where we define the trading strategy
  #Lets print the name of whats running
  runName <- paste0("Momentum Strategy with ", ema_smoothing, "/", mom_period)
  cat("Running Strategy: ", runName)
  
  #Find the column that contains the close price
  p <- Cl(primary)
  if (!missing(secondary)) {
    #Find the column that contains the close price
    s.Close <- Cl(secondary)
    
    #indicator series
    s.Ema <- EMA(s.Close, ema_smoothing)
    s.Momentum <- momentum(s.Ema, mom_period)
    s.Sig <- xts(vapply(s.Momentum, function(x) {
      ifelse(is.na(x), 0, ifelse(x > 0, 1, ifelse(x < 0, -1, 0)))
    }, FUN.VALUE = numeric(nrow(s.Momentum))), order.by = time(s.Momentum))
    
    p$Close.1 <- s.Close
    p$Ema <- s.Ema
    p$Momentum <- s.Momentum
    p$Sig <- s.Sig
    
    #Sync time with primary series and lag by 1 period
    p$Pos <- lag(na.locf(p$Sig))
    
  }
  else {
    #indicator series
    p$Ema <- EMA(p$Close, ema_smoothing)
    p$Momentum <- momentum(p$Ema, mom_period)
    p$Sig <- xts(vapply(p$Momentum, function(x) {
      ifelse(is.na(x), 0, ifelse(x > 0, 1, ifelse(x < 0, -1, 0)))
    }, FUN.VALUE = numeric(nrow(p$Momentum))), order.by = time(p$Momentum))
    
    #Lag signal by 1 period, becomes position column
    p$Pos <- lag(p$Sig)
    
  }
  
  invisible(p)
}


ZscoreStrat <-
  function(primary, secondary, ema_smoothing, z_period, threshold = 0) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Counter Z-Score Strategy with ", ema_smoothing, "/", z_period, "/", threshold)
    cat("Running Strategy: ", runName)
    
    #Find the column that contains the close price
    p <- Cl(primary)
    if (!missing(secondary)) {
      #Find the column that contains the close price
      s.Close <- Cl(secondary)
      
      #indicator series
      s.Ema <- EMA(s.Close, ema_smoothing)
      s.Zscore <- ZScore(s.Ema, z_period)
      
      s.Sig <- xts(vapply(s.Zscore, function(x, thres) {
        ifelse(is.na(x), 0, ifelse(x > thres, 1, ifelse(x < -thres, -1, 0)))
      }, thres = threshold, FUN.VALUE = numeric(nrow(s.Zscore))), order.by = time(s.Zscore))
      
      p$Close.1 <- s.Close
      p$Ema <- s.Ema
      p$Zscore <- s.Zscore
      p$Sig <- s.Sig
      
      #Sync time with primary series and lag by 1 period
      p$Pos <- lag(na.locf(p$Sig))
      
    }
    else {
      #indicator series
      p$Ema <- EMA(p$Close, ema_smoothing)
      p$Zscore <- ZScore(p$Ema, z_period)
      
      p$Sig <- xts(vapply(p$Zscore, function(x, thres) {
        ifelse(is.na(x), 0, ifelse(x > thres, 1, ifelse(x < -thres, -1, 0)))
      }, thres = threshold, FUN.VALUE = numeric(nrow(p$Zscore))), order.by = time(p$Zscore))
      
      #Lag signal by 1 period, becomes position column
      p$Pos <- lag(p$Sig)
      
    }
    
    invisible(p)
  }


CounterZscoreStrat <-
  function(primary, secondary, ema_smoothing, z_period, threshold = 0) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Counter Z-Score Strategy with ", ema_smoothing, "/", z_period, "/", threshold)
    cat("Running Strategy: ", runName)
    
    #Find the column that contains the close price
    p <- Cl(primary)
    if (!missing(secondary)) {
      #Find the column that contains the close price
      s.Close <- Cl(secondary)
      
      #indicator series
      s.Ema <- EMA(s.Close, ema_smoothing)
      s.Zscore <- ZScore(s.Ema, z_period)
      
      s.Sig <- xts(vapply(s.Zscore, function(x, thres) {
        ifelse(is.na(x), 0, ifelse(x > thres, -1, ifelse(x < -thres, 1, 0)))
      }, thres = threshold, FUN.VALUE = numeric(nrow(s.Zscore))), order.by = time(s.Zscore))
      
      p$Close.1 <- s.Close
      p$Ema <- s.Ema
      p$Zscore <- s.Zscore
      p$Sig <- s.Sig
      
      #Sync time with primary series and lag by 1 period
      p$Pos <- lag(na.locf(p$Sig))
      
    }
    else {
      #indicator series
      p$Ema <- EMA(p$Close, ema_smoothing)
      p$Zscore <- ZScore(p$Ema, z_period)
      
      p$Sig <- xts(vapply(p$Zscore, function(x, thres) {
        ifelse(is.na(x), 0, ifelse(x > thres, -1, ifelse(x < -thres, 1, 0)))
      }, thres = threshold, FUN.VALUE = numeric(nrow(p$Zscore))), order.by = time(p$Zscore))
      
      #Lag signal by 1 period, becomes position column
      p$Pos <- lag(p$Sig)
      
    }
    
    invisible(p)
  }

DiffusionStrat <-
  function(primary, secondary, ema_smoothing = 1, threshold = 0) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Diffusion Strategy with ", ema_smoothing, "/", threshold)
    cat("Running Strategy: ", runName)
    
    #Find the column that contains the close price
    p <- Cl(primary)
    if (!missing(secondary)) {
      #Find the column that contains the close price
      s.Close <- Cl(secondary)
      
      #indicator series
      s.Ema <- EMA(s.Close, ema_smoothing)
      
      s.Sig <- xts(vapply(s.Ema, function(x, thres) {
        ifelse(is.na(x), 0, ifelse(x > thres, 1, ifelse(x < -thres, -1, 0)))
      }, thres = threshold, FUN.VALUE = numeric(nrow(s.Ema))), order.by = time(s.Ema))
      
      p$Close.1 <- s.Close
      p$Ema <- s.Ema
      p$Sig <- s.Sig
      
      #Sync time with primary series and lag by 1 period
      p$Pos <- lag(na.locf(p$Sig))
      
    }
    
    invisible(p)
  }