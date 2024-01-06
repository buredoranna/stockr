## read data 
port <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)

aapl_nflx_cor <- cor(port$aapl, port$nflx)

diff_aapl <- ceiling(diff(range(port$aapl)))
diff_nflx <- ceiling(diff(range(port$nflx)))

if (diff_aapl > diff_nflx) {
	y_range_delta <- diff_aapl
} else {
	y_range_delta <- diff_nflx
}	

l <- lm(port$aapl~c(seq(1:length(port$aapl))))

plot(port$aapl, col="red", ylim=c(min(port$aapl), min(port$aapl) + y_range_delta),
     axes=F, ylab="", xlab=""); par(new=T)
abline(l, col="red"); par(new=T)
axis(2, labels=FALSE); par(new=T)

l <- lm(port$nflx~c(seq(1:length(port$nflx))))
plot(port$nflx, col="blue", ylim=c(min(port$nflx), min(port$nflx) + y_range_delta),
     axes=F, ylab="", xlab=c(paste(round(aapl_nflx_cor*100, 2), "% correlation")))
par(new=T)

abline(l, col="blue")
axis(1)

