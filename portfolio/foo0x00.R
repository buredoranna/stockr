require(stockPortfolio)

s.symbols <- c("uwti", "bib", "^gspc")

sdate <- "2015-02-10"
edate <- "2015-05-16"
cdate <- "2015-03-06"

n.shares.uwti <- 1000
n.shares.bib  <- 30

#returns <- getReturns(s.symbols, start = sdate, end = edate, freq = "day")

#current <- getReturns(s.symbols, start = cdate, end = cdate, freq = "day")

val.gspc <- rev(returns$full$"^gspc"$Close)
gross.ret.gspc <- val.gspc / val.gspc[1]
net.ret.gspc <- gross.ret.gspc - 1

val.uwti <- rev(returns$full$uwti$Close)
gross.ret.uwti <- val.uwti / val.uwti[1]
net.ret.uwti <- gross.ret.uwti - 1

val.bib <- rev(returns$full$bib$Close)
gross.ret.bib <- val.bib / val.bib[1]
net.ret.bib <- gross.ret.bib - 1

port.val <- n.shares.bib * val.bib + n.shares.uwti * val.uwti
gross.ret.port <- port.val / port.val[1]
net.ret.port <- gross.ret.port - 1

#mean.uwti   <- mean(returns$full$uwti$Close)
#stddev.uwti <-   sd(returns$full$uwti$Close)

#delta.stddevs.uwti <- (current$full$uwti$Close - mean.uwti) / stddev.uwti
#print(delta.stddevs.uwti)

yrange <- c(-0.5, 0.5)
xrange <- c(0, length(net.ret.port))

plot(net.ret.uwti, xlim = xrange, ylim = yrange, col = "darkblue")
par(new=T)
plot(net.ret.bib, xlim = xrange, ylim = yrange, col = "orange")
par(new=T)
plot(net.ret.port, xlim = xrange, ylim = yrange, col = "darkgreen")
par(new=T)
plot(net.ret.gspc, xlim = xrange, ylim = yrange, col = "darkred")
