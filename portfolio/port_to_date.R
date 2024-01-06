require(stockPortfolio)

n.shares.bib  <- 60
n.shares.ibb  <- 10
n.shares.cnxt <- 20
cash <- 827.51

s.symbols <- c("^gspc", "bib", "ibb", "cnxt")

sdate <- "2015-05-01"
edate <- "2015-06-05"

returns <- getReturns(s.symbols, start = sdate, end = edate, freq = "day")

w.oil     <- 0.3608
w.market  <- 0.3443
w.biotech <- 0.2949

## market returns
val.gspc <- rev(returns$full$"^gspc"$Adj.Close)
ret.gspc <- (val.gspc / val.gspc[1]) - 1

## biotech
val.ibb <- rev(returns$full$ibb$Adj.Close)
ret.ibb <- (val.ibb / val.ibb[1]) - 1

val.bib  <- rev(returns$full$bib$Adj.Close)
ret.bib  <- (val.bib / val.bib[1]) - 1

## china
val.cnxt <- rev(returns$full$cnxt$Adj.Close)

val.port <- n.shares.bib * val.bib + n.shares.ibb * val.ibb + n.shares.cnxt * val.cnxt + cash
ret.port <- val.port / val.port[1] - 1

xrange <- c(0, length(returns$full[[1]]$Adj.Close))
yrange <- c(-0.50, 0.50)

plot(ret.port, xlim = xrange, ylim = yrange)
#abline(h = mean(ret.reg.port))

