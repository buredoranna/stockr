## read data 
port <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)

aapl_goog_cor <- cor(port$aapl, port$goog)

diff_aapl <- ceiling(diff(range(port$aapl)))
diff_goog <- ceiling(diff(range(port$goog)))

if (diff_aapl > diff_goog) {
	y_range_delta <- diff_aapl
} else {
	y_range_delta <- diff_goog
}

print(paste("y-range delta:", y_range_delta))	

l <- lm(port$aapl~c(seq(1:length(port$aapl))))
plot(port$aapl, col="red", ylim=c(min(port$aapl), min(port$aapl) + y_range_delta),
     axes=F, ylab="", xlab=""); par(new=T)
abline(l, col="red"); par(new=T)
axis(2, labels=FALSE); par(new=T)

l <- lm(port$goog~c(seq(1:length(port$goog))))
plot(port$goog, col="blue", ylim=c(min(port$goog), min(port$goog) + y_range_delta),
     axes=F, ylab="", xlab=c(paste(round(aapl_goog_cor*100, 2), "% correlation")))
par(new=T)

abline(l, col="blue")
axis(1)

