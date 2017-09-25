es <- readr::read_csv('data/@ES.csv', col_names = FALSE) %>% mutate(DateTime = paste(X1, X2))

etf_list <- readr::read_csv('data/vanguard_etf_list.csv')
data <- purrr::map(etf_list[["SYMBOL"]], function(x) {getSymbols(x, src = "google",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                                                                 from = as.Date('2014-01-01'),
                                                                 to = Sys.Date(),
                                                                 auto.assign = FALSE)})

Closes <- purrr::map(data, function(x) Cl(x))
Closes <- do.call(merge, Closes)

primary <- Closes[,2] #VIS
secondary <- Closes[,3] #VFH

secondary <- to.weekly(secondary)

# RunBactest(strat)
ema_smoothing = 5
mom_period = 21
MomentumStrat <- function(primary, secondary, ema_smoothing, mom_period) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Momentum Strategy with ", ema_smoothing, "/", mom_period)
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
