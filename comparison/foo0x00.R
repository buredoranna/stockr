## compare upro combined with bib

require(stockPortfolio)

s.symbols <- c("spy", "iau")
sdate <- "2010-01-01"
edate <- "2015-02-19"

returns <- getReturns(s.symbols, start = sdate, end = edate, freq="day")

val.abc <- rev(returns$full[[1]]$Adj.Close)
ret.abc <- (val.abc / val.abc[1]) - 1

val.xyz <- rev(returns$full[[2]]$Adj.Close)	
ret.xyz <- (val.xyz / val.xyz[1]) - 1
