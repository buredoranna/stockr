# this draws a wireframe and density plot of a tech portfolio
# in "corwidth" sized blocks ( in days )
# the 1st point: cor betweeen cor(aapl(1:corwidth), foo(1:corwidth))
# library(lattice)

cwin <- 1:20  # number of days in the correlation cwindow
ndays <- 100 # number days into the past to go

corlist <- c()
meancor <- c()
mean_cor_twople <- c(0,0)
min_cor <- 1
yesterday <- 2
today <- 1

## generate a series of "read.table" commands
print("generating read.table list...")
system("./genreads.bash")
print("done")

# read in the data sets
print("reading data...")
source("readstocks.r")
print("done")

# open device for jpg output
print("generating images...")
system("rm *.jpg")
jpeg("img%00004d.jpg", width=1000, height=600, quality=100)
for (setn in ndays:1) {
	tm <- cbind(    a$Open[cwin], aapl$Open[cwin], adbe$Open[cwin], amat$Open[cwin],  amd$Open[cwin],
                 amzn$Open[cwin], brcd$Open[cwin], csco$Open[cwin], ebay$Open[cwin],  emc$Open[cwin],
                 goog$Open[cwin],  ibm$Open[cwin], intc$Open[cwin], intu$Open[cwin], jnpr$Open[cwin],
                 klac$Open[cwin],  lsi$Open[cwin], mrvl$Open[cwin], msft$Open[cwin], mxim$Open[cwin],
                 nflx$Open[cwin], ntap$Open[cwin], nvda$Open[cwin], orcl$Open[cwin],  stx$Open[cwin],
                 symc$Open[cwin],  txn$Open[cwin],  wdc$Open[cwin], xlnx$Open[cwin], yhoo$Open[cwin])

	## create correlation matrix and zero out the upper triangle
	tm_cors <- cor(tm)
	m <- matrix(nrow=nrow(tm_cors), ncol=ncol(tm_cors))
	m[upper.tri(m)] <- 0
	diag(m) <- 0
	m[lower.tri(m)] <- 1
	tm_cors <- m * tm_cors

	## mean cor of portfolio
	meancor <- mean(tm_cors[lower.tri(tm_cors)])
	
	## difference between yesterday and today's mean portfolio cor
	mean_cor_twople[yesterday] <- mean_cor_twople[today]
	mean_cor_twople[today] <- meancor
	mean_cor_slope <- mean_cor_twople[today] - mean_cor_twople[yesterday]
	
	## 1-row 2-columns per plot-page
	par(mfrow=c(1,2))
	
	## wireframe surface plot
	persp(tm_cors, zlab="cor", zlim=c(-1,1),
              main=paste("today -", setn,
                         "\nmean cor: ", format(round(meancor, 4), nsmall=4),
                         "\n2-day mean cor slope: ", format(round(mean_cor_slope, 3), nsmall=4)),
              theta=30, phi=30, expand=0.1, shade=0.5, col="green", mar=c(0,0,0,0))

	## histogram
	# hist(tm_cors[lower.tri(tm_cors)], breaks=c(seq(-1,1,0.1)), ylim=c(0,60), col="green")
	
	## density plot with rug
	d <- density(tm_cors[lower.tri(tm_cors)])
	plot(d, xlim=c(-1,1), ylim=c(0,1.5))
	polygon(d, col="darkgreen")
	rug(tm_cors[lower.tri(tm_cors)], col="darkblue")
	
	cwin <- cwin + 1
}
print("done")
dev.off()
print("graphics device closed")

# system("convert -delay 50 *.jpg out.gif")
system("rm wiredensity.avi")
system("avconv -r 15 -i img%00004d.jpg -crf 18 -c:v rawvideo wiredensity.avi")
system("rm *.jpg")


