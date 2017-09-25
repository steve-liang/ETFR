source('common.R')
source('strategies.R')

Chart.2 <- function(timeseries1, timeseries2) {
  
  combined <- cbind(Cl(timeseries1), Cl(timeseries2))
  combined <- na.locf(combined)
  
  colnames(combined) <- c('Primary', 'Secondary')
  dygraph(combined) %>%
    dyAxis("y", label = "Primary Series") %>%
    dyAxis("y2", label = "Secondary Series", independentTicks = T) %>%
    dySeries("Secondary", axis = 'y2')
}

Plot_Result <- function(ohlc, r) {
  
  ind <- momentum(EMA(Cl(ohlc), 5), 30)
    
  td <- r$Trades[r$Trades != ""]
  bd <- rbind(td[grep("Buy", td)], td[grep("Cover", td)])
  sd <- td[grep("Sell", td)]

  bdf <- as.data.frame(list(x = time(bd), tooltip = "", text = "B"))
  sdf <- as.data.frame(list(x = time(sd), tooltip = "", text = "S"))
  hc <- highcharter::highchart(type = "stock") %>%
    hc_yAxis_multiples(
      create_yaxis(2, height = c(3, 1), turnopposite = TRUE)) %>% 
    hc_add_series(ohlc, yAxis = 0, id = "price", type = "ohlc") %>%
    hc_add_series(ind, id = "momentum", yAxis = 1, type = "column") %>%
    hc_add_series(bdf, hcaes(x = x),
                  type = "flags", shape = "squarepin", title = "B", onSeries = "price") %>%
    hc_add_series(sdf, hcaes(x = x), type = "flags", shape = "flag", title = "S", 
                  onSeries = "price")
  
  hc
  
}

# RunBacktest takes strategy output (Close/Ema/Momentum/Sig/Pos) as input
# Output is a 2-object list, Result and Trade. Result contains: Period to period return; Close price; and Position.
# Trade contains the trade instrument as strings (Reverse.Buy/SellShort, Buy/SellShort/etc)
# The Result object is used for performance calculation
# Result$Pos is the series for diffusion index or correlation calculation
Run_Backtest <- function(strat) {
    firstTradeIndex <- min(which(strat$Pos != 0 & !is.na(strat$Close)))
    firstTradeDate <- time(strat[firstTradeIndex])
    lastTradeIndex <- max(which(strat$Pos != 0 & !is.na(strat$Close)))
    lastTradeDate <- time(strat[lastTradeIndex])
    
    #subsetting period that had trades
    bt <- strat[firstTradeIndex:lastTradeIndex]
    
    #safely fill values
    bt <- na.locf(bt)
    bt$Ret <- ROC(bt$Close, type = 'continuous') * bt$Pos
    bt$Ret[is.na(bt$Ret)] <- 0
    chart.CumReturns(bt$Ret, geometric = F, wealth.index = T)
    
    temp <- cbind(bt$Pos, bt$Pos - lag(bt$Pos))
    
    trades <- xts(
      ifelse(is.na(temp$Pos.1) & temp$Pos == -1, "SellShort",
             ifelse(is.na(temp$Pos.1) & temp$Pos == 1, "Buy",
                    ifelse(temp$Pos.1 == 2, "Reverse.Buy",
                           ifelse(temp$Pos.1 == -2, "Reverse.SellShort",
                                  ifelse(temp$Pos.1 == 1 & temp$Pos == 1, "Buy",
                                         ifelse(temp$Pos.1 == 1 & temp$Pos == 0, "Cover",
                                                ifelse(temp$Pos.1 == -1 & temp$Pos == -1, "SellShort",
                                                       ifelse(temp$Pos.1 == -1 & temp$Pos == 0, "Sell", "")))))))),
      order.by = time(temp))
    
    #Actually trade was last Bar end
    trades <- stats::lag(trades, -1)
    colnames(trades) <- c("Trade")
    
    Performance.Stats(bt$Ret, trades)
    
    out <- list("Result" = cbind(bt$Ret, bt$Close, bt$Pos), "Trades" = trades)
    
    invisible(out)
  }


Performance.Stats <- function(bt.returns, bt.trades) {
  
  cror <- Return.annualized(bt.returns, geometric = F)
  vol <- sd.annualized(bt.returns, scale = 252)
  sharpe <- SharpeRatio.annualized(bt.returns, geometric = F)
  trades <- length(grep("Reverse", bt.trades)) + length(grep("Cover", bt.trades)) + +length(grep("\\bSell\\b", bt.trades))
  
  Output <- rbind(as.character(round(sharpe, digits = 4)),
                  as.character(round(cror, digits = 4)),
                  as.character(round(vol, digits = 4)),
                  as.character(round(trades, digits = 0)),
                  as.character(min(time(bt.trades))),
                  as.character(max(time(bt.trades))))
  colnames(Output) = c('Performance Stats')
  rownames(Output) <- c('Sharpe', 'CROR', 'Volatiltiy', 'Total Trades', 'Start Date', 'End Date')
  print(Output, quote = F)
  
}

ExportPosToFile <- function(pos, path) {
  write.zoo(pos, path, col.names = F, sep = ",")
}