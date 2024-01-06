require(stockPortfolio)

symbols <- c("spy", "uso", "uwti")
sdate   <- "2013-01-01"
edate   <- "2015-02-13"

returns <- getReturns(symbols, start = sdate, end = edate, freq="month")

val.spy <- rev(returns$full$spy$Adj.Close)
ret.spy <- (val.spy / val.spy[1]) - 1
gross.spy <- (val.spy / val.spy[1])

val.uwti <- rev(returns$full$uwti$Adj.Close)
#ret.uwti <- 

