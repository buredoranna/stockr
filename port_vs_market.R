require(stockPortfolio)

s.date <- "2015-02-10"
e.date <- "2015-04-27"

w.uwti <- 0.4
w.bib  <- 0.4
w.cash <- 1 - w.uwti - w.bib

s.symbols <- c("uwti", "bib", "^gspc")

s.returns <- getReturns(s.symbols, start = s.date, end = e.date, freq = "day")


val.uwti <- rev(s.returns$full$uwti$Adj.Close)
ret.uwti <- val.uwti / val.uwti[1]

val.bib <- rev(s.returns$full$bib$Adj.Close)
ret.bib <- val.bib / val.bib[1]

val.gspc <- rev(s.returns$full$"^gspc"$Adj.Close)
ret.gspc <- val.gspc / val.gspc[1]

ret.port <- 0.5 * ret.uwti + 0.5 * ret.bib

