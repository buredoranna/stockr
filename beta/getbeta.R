## returns beta between two securities

require(stockPortfolio)

s.symbols <- c("spy", "iau")
s.symbols <- c("gdx", "spy")
s.symbols <- c("ibb", "bib", "upro", "spy", "uwti", "uso")

sdate <- "2013-01-01"
edate <- Sys.Date()
edate <- "2014-01-01"

returns <- getReturns(s.symbols, start = sdate, end = edate, freq="day")

val.ibb  <- rev(returns$full$ibb$Adj.Close)
ret.ibb  <- (val.nbi / val.ibb[1]) - 1

val.bib  <- rev(returns$full$bib$Adj.Close)
ret.bib  <- (val.bib / val.bib[1]) - 1
w.bib    <- 0.2949

val.upro <- rev(returns$full$upro$Adj.Close)
ret.upro <- (val.upro / val.upro[1]) - 1
w.upro   <- 0.3443

val.spy  <- rev(returns$full$spy$Adj.Close)
ret.spy  <- (val.spy / val.spy[1]) - 1

val.uwti <- rev(returns$full$uwti$Adj.Close)
ret.uwti <- (val.uwti / val.uwti[1]) - 1
w.uwti   <- 0.3608

val.uso  <- rev(returns$full$uso$Adj.Close)
ret.uso  <- (val.uso / val.uso[1]) - 1

ret.port <- w.bib * ret.bib + w.upro * ret.upro + w.uwti * ret.uwti



