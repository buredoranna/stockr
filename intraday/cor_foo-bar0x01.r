## read data 
port <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)

nflx_csco_cor <- cor(port$nflx, port$csco)

diff_nflx <- ceiling(diff(range(port$nflx)))
diff_csco <- ceiling(diff(range(port$csco)))

if (diff_nflx > diff_csco) {
	y_range_delta <- diff_nflx
} else {
	y_range_delta <- diff_csco
}	

l <- lm(port$nflx~c(seq(1:length(port$nflx))))

plot(port$nflx, main="nflx (r) : csco (b)", col="red",
     ylim=c(min(port$nflx), min(port$nflx) + y_range_delta), axes=F, ylab="", xlab="")
par(new=T)
abline(l, col="red"); par(new=T)
axis(2, labels=FALSE); par(new=T)

l <- lm(port$csco~c(seq(1:length(port$csco))))
plot(port$csco, col="blue", ylim=c(min(port$csco), min(port$csco) + y_range_delta),
     axes=F, ylab="", xlab=c(paste(round(nflx_csco_cor*100, 2), "% correlation")))
par(new=T)

abline(l, col="blue")
par(new=T)
axis(1)
par(new=T)

