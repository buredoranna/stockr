## this displays the returns of two stocks comapred to one another from a start date to and end
## date over a given n-day window

require(stockPortfolio)

s.symbols <- c("bib", "drn")

sdate <- "2012-01-01"
edate <- Sys.Date()

returns <- getReturns(s.symbols, start = sdate, end = edate, freq="day")

val.abc <- rev(returns$full[[1]]$Adj.Close)
ret.abc <- (val.abc / val.abc[1]) - 1

val.xyz <- rev(returns$full[[2]]$Adj.Close)
ret.xyz <- (val.xyz / val.xyz[1]) - 1

print(cor(ret.abc, ret.xyz))


plot.window <- c(1:20)

while (max(plot.window) <= length(val.abc)) {
  ret.abc <- (val.abc[plot.window] / val.abc[min(plot.window)]) - 1
  ret.xyz <- (val.xyz[plot.window] / val.xyz[min(plot.window)]) - 1
  
  yrange <- range(ret.abc, ret.xyz)
  yrange <- c(-0.5, 0.5)
  xrange <- c(0, length(ret.abc))
  
  mindate <- rev(returns$full[[1]]$Date)[min(plot.window)]
  maxdate <- rev(returns$full[[1]]$Date)[max(plot.window)]
  main.string <- paste(mindate, "::", maxdate)

  plot(ret.abc, ylim = yrange, xlim = xrange, type="l", col = "orange")
  par(new=T)
  plot(ret.xyz, ylim = yrange, xlim = xrange, type="l", col = "darkblue", main = main.string)
  readline("press the any key to continue")
  plot.window <- plot.window + 1
}

