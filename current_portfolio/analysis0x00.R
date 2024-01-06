require(stockPortfolio)

sdate <- "2014-02-11"
edate <- "2015-02-11"

symbols <- c("upro", "voo", "uwti", "uso")

w.uwti <- 0.51
w.upro <- 0.49

returns <- getReturns(symbols, start = sdate, end = edate, freq = "day")

val.upro <- rev(returns$full$upro$Adj.Close)
val.uwti <- rev(returns$full$uwti$Adj.Close)
val.voo  <- rev(returns$full$voo$Adj.Close)
val.uso  <- rev(returns$full$uso$Adj.Close)

current.val.upro <- tail(val.upro,1)
current.val.uwti <- tail(val.uwti,1)

grossret.upro <- val.upro / val.upro[1]
grossret.uwti <- val.uwti / val.uwti[1]
grossret.voo  <- val.voo  / val.voo[1]
grossret.uso  <- val.uso  / val.uso[1]

grossret.port <- w.uwti * grossret.uwti + w.upro * grossret.upro

xrange <- c(0, max(length(grossret.upro), length(grossret.uwti), length(grossret.port)))
yrange <- range(grossret.upro - 1, grossret.uwti - 1, grossret.port - 1)

plot(grossret.upro - 1, xlim=xrange, ylim=yrange, col="darkred", lwd=1, type="l")
par(new=T)
plot(grossret.uwti - 1, xlim=xrange, ylim=yrange, col="orange", lwd=1, type="l")
par(new=T)
plot(grossret.port - 1, xlim=xrange, ylim=yrange, col="darkgreen", lwd=1, type="l")

cat(paste("### beta:", sdate, edate, "###\n"))
beta <- cov(grossret.uwti, grossret.upro) / var(grossret.upro)
print(beta)



