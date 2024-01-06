# plot the value of the portfolio

## generate a series of "read.table" commands
#system("./genreads.bash")

## read in the data sets
# source("readstocks.r")

sd    <- 1000 # number of days into past to start at. (100 = "start 100 days into the past")
ndays <- 20   # number of days per plot window

portval <- c() # portfolio value
runtot <- c()

## remove any previous images and animations
system("rm *.jpg")
system("rm portval.avi")

## open image device for output
jpeg("img%00004d.jpg", width=600, height=600, quality=100)
while (sd - ndays > 1) {
	d <- sd
	# build a set of portfolio values to plot
	portval <- c()
	for (n in 1:ndays) {
		val <-    a$Open[d] + aapl$Open[d] + adbe$Open[d] + amat$Open[d] +  amd$Open[d] +
               amzn$Open[d] + brcd$Open[d] + csco$Open[d] + ebay$Open[d] +  emc$Open[d] +
               goog$Open[d] +  ibm$Open[d] + intc$Open[d] + intu$Open[d] + jnpr$Open[d] + 
               klac$Open[d] +  lsi$Open[d] + mrvl$Open[d] + msft$Open[d] + mxim$Open[d] +
               nflx$Open[d] + ntap$Open[d] + nvda$Open[d] + orcl$Open[d] +  stx$Open[d] +
               symc$Open[d] +  txn$Open[d] +  wdc$Open[d] + xlnx$Open[d] + yhoo$Open[d]
		portval <- append(portval, val)
		d <- d - 1
	}
	# plot(portval, ylim=c(min(portval) - 100, max(portval) + 100), type="l")
	# boxplot(portval)
	# m <- matrix(portval, nrow=1, ncol=length(portval))

	# barplot(portval, ylim=c(min(portval), max(portval)), col="darkgreen")
	runtot <- append(runtot, portval[length(portval)])
	# runtot <- append(runtot, portval[1])
	# xwin <- c(length(portval), 0)
	plot(runtot, ylim=c(min(runtot), max(runtot)), type="l", lwd="2", col="darkgreen")
	
	# xwin <- c(length(runtot) - ndays, length(runtot)) 
	# plot(runtot, ylim=c(min(runtot), max(runtot)), xlim=xwin, type="l", lwd="2", col="darkgreen")
	# barplot(runtot, ylim=c(min(runtot), max(runtot)), col="darkgreen")
	# barplot(portval, col="darkgreen")
	
	# x <- c(seq(1,ndays))
	# polygon(c(x,portval), col="darkgreen")
	sd <- sd - 1
}

## close image device
dev.off()

system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo portval.avi")
system("rm *.jpg")

	

