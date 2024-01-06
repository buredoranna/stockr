# plot the value of the portfolio

## generate a series of "read.table" commands
#system("./genreads.bash")

## read in the data sets
# source("readstocks.r")

sd <- 1000 # number of days into past to start at. (100 = "start 100 days into the past")

portval <- c() # portfolio value

## remove any previous images and animations
system("rm *.jpg")
system("rm portval.avi")

## open image device for output
jpeg("img%00004d.jpg", width=600, height=600, quality=100)
#while (sd - ndays > 1) {
for (d in sd:1) {
	# build a set of portfolio values to plot
	val <-    a$Open[d] + aapl$Open[d] + adbe$Open[d] + amat$Open[d] +  amd$Open[d] +
           amzn$Open[d] + brcd$Open[d] + csco$Open[d] + ebay$Open[d] +  emc$Open[d] +
           goog$Open[d] +  ibm$Open[d] + intc$Open[d] + intu$Open[d] + jnpr$Open[d] + 
           klac$Open[d] +  lsi$Open[d] + mrvl$Open[d] + msft$Open[d] + mxim$Open[d] +
           nflx$Open[d] + ntap$Open[d] + nvda$Open[d] + orcl$Open[d] +  stx$Open[d] +
           symc$Open[d] +  txn$Open[d] +  wdc$Open[d] + xlnx$Open[d] + yhoo$Open[d]
	portval <- append(portval, val)	
	
	if (length(portval) > 1) {
		l <- lm(portval~seq(1, length(portval)))
		abline(l)
	}
	par(pin=c(5,2))
	plot(portval, ylim=c(min(portval), max(portval)),
         main=paste("slope: ", round(l$coeff[[2]], 2)),
         type="l", lwd="2", col="darkgreen")
}

## close image device
dev.off()

system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo portval.avi")
system("rm *.jpg")

	

