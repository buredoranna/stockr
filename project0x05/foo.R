require(stockPortfolio)
require(zoo)

n.day.roll.window <- 20

mystocks <- c("^gspc", "^vix")

mydata <- getReturns(mystocks, start = "2013-01-01", end = "2015-06-01", freq = "day")

val.stock <- rev(mydata$full[[1]]$Adj.Close)
ret.stock <- val.stock / val.stock[1]

roll.mean.stock   <- rollmean(ret.stock, n.day.roll.window)
roll.stddev.stock <- rollapply(ret.stock, n.day.roll.window, var)

val.vix <- rev(mydata$full[[2]]$Adj.Close)
ret.vix <- val.stock / val.stock[2]

roll.mean.vix   <- rollmean(ret.vix, n.day.roll.window)
roll.stddev.vix <- rollapply(ret.vix, n.day.roll.window, var)

ymin <- 0
ymax <- max(roll.mean.stock)
xmin <- 0
xmax <- max(roll.stddev.stock)

xrange <- c(xmin, xmax)
yrange <- c(ymin, ymax)

jpeg("img%000005d.jpg", width = 600, height = 600, quality = 100)
xlabel <- "std dev"
ylabel <- "mean"
for (n in 1:length(roll.mean.stock)) {
  plot(roll.stddev.stock[1:n], roll.mean.stock[1:n], ylim = yrange, xlim = xrange,
      xlab = xlabel,
      ylab = ylabel)
  par(new=T)
  plot(roll.stddev.stock[n], roll.mean.stock[n], ylim = yrange, xlim = xrange, col = "red",
       xlab = xlabel,
       ylab = ylabel)
  
}
dev.off()
