## read data 
port <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)

aapl_amzn_cor <- cor(port$aapl, port$amzn)

diff_aapl <- ceiling(diff(range(port$aapl)))
diff_amzn <- ceiling(diff(range(port$amzn)))

if (diff_aapl > diff_amzn) {
	y_range_delta <- diff_aapl
} else {
	y_range_delta <- diff_amzn
}	

l <- lm(port$aapl~c(seq(1:length(port$aapl))))

plot(port$aapl, main="aapl (r) : amzn (b)", col="red",
     ylim=c(min(port$aapl), min(port$aapl) + y_range_delta), axes=F, ylab="", xlab="")
par(new=T)
abline(l, col="red"); par(new=T)
axis(2, labels=FALSE); par(new=T)

l <- lm(port$amzn~c(seq(1:length(port$amzn))))
plot(port$amzn, col="blue", ylim=c(min(port$amzn), min(port$amzn) + y_range_delta),
     axes=F, ylab="", xlab=c(paste(round(aapl_amzn_cor*100, 2), "% correlation")))
par(new=T)

abline(l, col="blue")
par(new=T)
axis(1)
par(new=T)

