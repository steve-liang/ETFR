library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(dygraphs)

CSI_HEADER <- c('Open', 'High', 'Low', 'Close', 'Volume', 'OpInt', 'Expiration', 'Unadjusted')
ASCII_HEADER <- c('Close')
TB_HEADER <- c('Open', 'High', 'Low', 'Close', 'Volume', 'OpInt')


#Make sure the date column is in one of the three formats: 2010-01-04, 20100104, 1/4/2010
To.Date <- function(date_str) {
  ifelse(!is.na(as.Date(as.character(date_str), format = '%Y-%m-%d')) | !is.na(as.Date(as.character(date_str), format = '%Y%m%d')), return(ymd(date_str)),
         ifelse(!is.na(as.Date(as.character(date_str), format = '%m/%d/%Y')), return(mdy(date_str)), NA))
}

CsiBarLoader <- function(filepath, returnXTS = T) {
  df <- read.csv(filepath, header = F)
  
  if (returnXTS == T) {
    
    df <- as.xts(df[, -1], OHLC = T, order.by = To.Date(df[, 1]))
    colnames(df) <- CSI_HEADER
  }
  return(df)
}


AsciiBarLoader <- function(filepath, returnXTS = T) {
  df <- read.csv(filepath, header = F)
  
  if (returnXTS == T) {
    df <- as.xts(df[, -1], To.Date(df[, 1]))
    colnames(df) <- ASCII_HEADER
  }
  return(df)
}

TBBarLoader <- function(filepath, isIntraday = F, returnXTS = T) {
  df <- read.csv(filepath, header = F)
  if (returnXTS == T) {
    if (!isIntraday) {
      df <- as.xts(df[, -1], order.by = To.Date(df[, 1]))
    } else {
      df <- as.xts(df[, -1], order.by = as.POSIXct(df[, 1], format = '%Y/%m/%d %H:%M'))
      
    }
    colnames(df) <- TB_HEADER
  }
  return(df)
}

xts.sync <- function(low.freq, high.freq)   #only able to sync 1-dimensional series
{
  combined <- cbind(low.freq, high.freq)
  return(na.locf(combined[, 1]))
}

ZScore <- function(myxts, n = 100) {
  colnames(myxts) <- "Z.Score"
  last_scale <- function(df) {
    return(last(scale(df)))
  }
  return(rollapply(myxts, n, last_scale))
}

seasonality <- function(data, fmt = "%H:%M", FUN = mean) {
  # aggregate mean, sd stats for data group by time 
  z <- aggregate(data, format(index(data), fmt), function(x) FUN(x))
  
  barplot(z, main = "Seaonality Study")
  abline(h = mean(z) + sd(z), col = "blue")
  out <- list("Result" = z, "Triggerline" = mean(z) + sd(z))
  invisible(out)
}

dummy.xts <- function(N, st = "2012/01/01", et = "2012/12/31", col.Name = T) {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et, st, units = "secs"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
  x <- xts(rep(0, length(rt)), order.by = rt)
  second(index(x)) = 0
  if (col.Name) {
    colnames(x) = "Close"
  }
  return(x)
}