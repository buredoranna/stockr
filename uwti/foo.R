require(stockPortfolio)

sdate = "2015-02-01"
edate = "2015-05-19"

mydata <- getReturns(c("uwti"), start = sdate, end = edate, freq = "day")


