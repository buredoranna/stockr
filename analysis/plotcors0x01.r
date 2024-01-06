# this plots the correlation between aaple and several other stocks
# in "corwidth" sized blocks ( in days )
# the 1st point: cor betweeen cor(aapl(1:corwidth), foo(1:corwidth))

cwin <- 1:20 # number of days to use for correlation calculation
ndays <- 1 # number days into the past to go
daysperplot <- 90
xgridlines <- 5 # gridlines

## generate a series of "read.table" commands
# system("./genreads.bash")

## read in stock data
# source("readstocks.r")

## read in color pallete
source("pallete.r")

aapl_amzn <- c()
aapl_goog <- c()
aapl_ibm  <- c()
aapl_nflx <- c()
aapl_csco <- c()

## generate set of correlations
cm <- matrix(nrow=daysperplot, ncol=5)
for (d in 1:daysperplot) {
	## bind together a ticker matrix window
	aapl_amzn <- cor(aapl$Open[cwin], amzn$Open[cwin])
	aapl_goog <- cor(aapl$Open[cwin], goog$Open[cwin])	
	aapl_ibm  <- cor(aapl$Open[cwin],  ibm$Open[cwin])
	aapl_nflx <- cor(aapl$Open[cwin], nflx$Open[cwin])
	aapl_csco <- cor(aapl$Open[cwin], csco$Open[cwin])
	
	corlist <- c(aapl_amzn, aapl_goog, aapl_ibm, aapl_nflx, aapl_csco)

	cm[d,] <- corlist
	cwin <- cwin + 1
}

yrange <- c(-1,1)

plot(cm[,1], col=amzn_col, ylim=yrange, xlab="", ylab=""); par(new=T)
plot(amzn$Open[cwin], col=amzn_col, axes=F, xlab="", ylab="", type="l"); par(new=T)

plot(cm[,2], col=goog_col, ylim=yrange, axes=F, xlab="", ylab=""); par(new=T)
plot(goog$Open[cwin], col=goog_col, axes=F, xlab="", ylab="", type="l"); par(new=T)

plot(cm[,3], col=ibm_col, ylim=yrange, axes=F, xlab="", ylab=""); par(new=T);
plot(ibm$Open[cwin], col=ibm_col, axes=F, xlab="", ylab="", type="l"); par(new=T)

#plot(cm[,4], col=nflx_col, ylim=yrange, axes=F, xlab="", ylab=""); par(new=T);
#plot(nflx$Open[cwin], col=nflx_col, axes=F, xlab="", ylab="", type="l"); par(new=T)

#plot(cm[,5], col=csco_col, ylim=yrange, axes=F, xlab="", ylab=""); par(new=T);
#plot(csco$Open[cwin], col=csco_col, axes=F, xlab="", ylab="", type="l"); par(new=T)

plot(aapl$Open[cwin], axes=F, ylab="", xlab="", type="l", lwd=2); par(new=T)

abline(h=c(0), v=c(seq(0, daysperplot, xgridlines))); par(new=T)
# axis(1, at=c(seq(0, daysperplot, cwin)))



	
	
