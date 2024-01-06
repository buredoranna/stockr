# this plots the correlation between aaple and several other stocks
# the x-axis represents the depth ( in number of days ) used to 
# calculate the correlation 

# read in stock data
#aapl <- read.table("/home/user/data/stocks/aapl.csv", sep=",", header=T)
#amzn <- read.table("/home/user/data/stocks/amzn.csv", sep=",", header=T)
#ebay <- read.table("/home/user/data/stocks/ebay.csv", sep=",", header=T)
#goog <- read.table("/home/user/data/stocks/goog.csv", sep=",", header=T)
#ibm  <- read.table("/home/user/data/stocks/ibm.csv" , sep=",", header=T)
#msft <- read.table("/home/user/data/stocks/msft.csv", sep=",", header=T)
#orcl <- read.table("/home/user/data/stocks/orcl.csv", sep=",", header=T)
#yhoo <- read.table("/home/user/data/stocks/yhoo.csv", sep=",", header=T)

# this must exist to be referenced later
# end-date, correlation
aapl_amzn_cor <- c()
aapl_ebay_cor <- c()
aapl_goog_cor <- c()
aapl_ibm_cor  <- c()
aapl_msft_cor <- c()
aapl_orcl_cor <- c()
aapl_yhoo_cor <- c()

flr <- 2
for (s in 1:750) {
	aapl_amzn_cor[s] <- cor(aapl$Close[1:flr], amzn$Close[1:flr])
	aapl_ebay_cor[s] <- cor(aapl$Close[1:flr], ebay$Close[1:flr])
	aapl_goog_cor[s] <- cor(aapl$Close[1:flr], goog$Close[1:flr])
	aapl_ibm_cor[s]  <- cor(aapl$Close[1:flr],  ibm$Close[1:flr])
	aapl_msft_cor[s] <- cor(aapl$Close[1:flr], msft$Close[1:flr])
	aapl_orcl_cor[s] <- cor(aapl$Close[1:flr], orcl$Close[1:flr])
	aapl_yhoo_cor[s] <- cor(aapl$Close[1:flr], yhoo$Close[1:flr])
	flr <- flr + 1
}

# plots
yrange <- c(-1,1)

plot(aapl_amzn_cor, type="l", col="red", ylim=yrange, ylab="cumulative correlation", xlab="depth in days", main="apple cor with R:amzn, G:ebay, B: goog, O:ibm, Brn: msft, DrkMag: orcl, Cyan: yhoo")
par(new=T)
plot(aapl_ebay_cor, type="l", col="green", ylim=yrange, ylab="", xlab="")
par(new=T)
plot(aapl_goog_cor, type="l", col="blue", ylim=yrange, ylab="", xlab="")
par(new=T)
plot(aapl_ibm_cor, type="l", col="orange", ylim=yrange, ylab="", xlab="")
par(new=T)
plot(aapl_msft_cor, type="l", col="brown", ylim=yrange, ylab="", xlab="")
par(new=T)
plot(aapl_orcl_cor, type="l", col="darkmagenta", ylim=yrange, ylab="", xlab="")
par(new=T)
plot(aapl_yhoo_cor, type="l", col="cyan",ylim=yrange, ylab="", xlab="")

