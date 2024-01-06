require(stockPortfolio)

s.symbols <- c("uso", "uco", "uwti")
sdate <- "2010-01-01"
edate <- "2015-02-26"

returns <- getReturns(s.symbols, start = sdate , end = edate, freq = "day")


