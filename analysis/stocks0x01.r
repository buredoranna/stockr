# this plots the correlation between aaple and several other stocks
# in "corwidth" sized blocks ( in days )
# the 1st point: cor betweeen cor(aapl(1:corwidth), foo(1:corwidth))

corwidth <- 20 # number of days to use for correlation calculation
ndays <- 90 # number days into the past to go
xgridlines <- 5

# generate reads
# system("./genreads.bash")

# read in stock data
# source("readstocks.r")

# colors
aapl_col <- "black"
amd_col  <- "green"
amzn_col <- "green"
ebay_col <- "red"
goog_col <- "red"
ibm_col  <- "blue"
intc_col <- "violetred"
msft_col <- "limegreen"
orcl_col <- "magenta"
yhoo_col <- "yellow"


# this must exist to be referenced later
# end-date, correlation
aapl_amzn_cor <- c()
aapl_amd_cor  <- c()
aapl_ebay_cor <- c()
aapl_goog_cor <- c()
aapl_ibm_cor  <- c()
aapl_intc_cor <- c()
aapl_msft_cor <- c()
aapl_orcl_cor <- c()
aapl_yhoo_cor <- c()

ceil <- 1
flr <- ceil + corwidth
for (s in 1:ndays) {
	aapl_amzn_cor[s] <- cor(aapl$Open[ceil:flr], amzn$Open[ceil:flr])
	aapl_amd_cor[s]  <- cor(aapl$Open[ceil:flr],  amd$Open[ceil:flr])
	aapl_ebay_cor[s] <- cor(aapl$Open[ceil:flr], ebay$Open[ceil:flr])
	aapl_goog_cor[s] <- cor(aapl$Open[ceil:flr], goog$Open[ceil:flr])
	aapl_ibm_cor[s]  <- cor(aapl$Open[ceil:flr],  ibm$Open[ceil:flr])
	aapl_intc_cor[s] <- cor(aapl$Open[ceil:flr], intc$Open[ceil:flr])
	aapl_msft_cor[s] <- cor(aapl$Open[ceil:flr], msft$Open[ceil:flr])
	aapl_orcl_cor[s] <- cor(aapl$Open[ceil:flr], orcl$Open[ceil:flr])
	aapl_yhoo_cor[s] <- cor(aapl$Open[ceil:flr], yhoo$Open[ceil:flr])
	ceil <- ceil + 1
	flr <- flr + 1
}

# set this to align y-axis accross multiple plots
yrange <- c(-1,1)

# main title for graph
title <- paste("aaple cor with AMZN_",amzn_col," GOOG_",goog_col," IBM_",ibm_col, sep="")

plot(aapl_amzn_cor,  col=amzn_col, ylim=yrange, ylab="cumulative correlation", xlab=paste("days past present. corwidth = ", corwidth, "days."), main=title); par(new=T)
#plot(aapl_amd_cor,  col="orchid", ylim=yrange, ylab="", xlab=""); par(new=T)
#plot(aapl_ebay_cor,  col="green", ylim=yrange, ylab="", xlab=""); par(new=T)
plot(aapl_goog_cor, col=goog_col, ylim=yrange, ylab="", xlab=""); par(new=T)
plot(aapl_ibm_cor,  col=ibm_col,  ylim=yrange, ylab="", xlab=""); par(new=T)
#plot(aapl_intc_cor,  col="peru", ylim=yrange, ylab="", xlab=""); par(new=T)
#plot(aapl_msft_cor,  col="brown", ylim=yrange, ylab="", xlab=""); par(new=T)
#plot(aapl_orcl_cor,  col="darkmagenta", ylim=yrange, ylab="", xlab=""); par(new=T)
#plot(aapl_yhoo_cor,  col="cyan", bg="darkgrey", ylim=yrange, ylab="", xlab=""); par(new=T)
abline(h=c(0), v=c(seq(0,ndays,xgridlines))); par(new=T)
axis(1, at=c(seq(0,ndays, corwidth))); par(new=T)

plot(aapl$Open[1:ndays], axes=F, xlab="", ylab="", type="l", lwd=2); par(new=T)
plot(amzn$Open[1:ndays], axes=F, xlab="", ylab="", type="l", col=amzn_col); par(new=T)
plot(goog$Open[1:ndays], axes=F, xlab="", ylab="", type="l", col=goog_col); par(new=T)
plot( ibm$Open[1:ndays], axes=F, xlab="", ylab="", type="l", col=ibm_col) 

