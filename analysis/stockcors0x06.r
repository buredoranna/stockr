# this calculations various attributes of a portfolio
# and if desired, creates plots 

## tm = "ticker matrix"

sd <- 2000 # number days into the past to start
dicw <- 20  # days in correlation window
cwin <- sd:(sd - dicw) # window for correlation calculation

drawplots <- 1 # 1-draw, 0-DO NOT draw plots

portval <- c() # storage for portfolio value
corlist <- c() # storage for correlations
meancor <- c() # storage for mean correlation
meancor_list <- c()
slopelist <- c()

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read in the data sets
print("reading data...")
source("readstocks.r")
print("done")

# open device for jpg output
print("generating images...")
system("rm *.jpg")
jpeg("img%00004d.jpg", width=1200, height=600, quality=100)
while (sd - dicw >= 1) {
	## bind together a matrix of ticker opening values
	tm <- cbind(    a$Open[cwin], aapl$Open[cwin], adbe$Open[cwin], amat$Open[cwin],  amd$Open[cwin],
                 amzn$Open[cwin], brcd$Open[cwin], csco$Open[cwin], ebay$Open[cwin],  emc$Open[cwin],
                 goog$Open[cwin],  ibm$Open[cwin], intc$Open[cwin], intu$Open[cwin], jnpr$Open[cwin],
                 klac$Open[cwin],  lsi$Open[cwin], mrvl$Open[cwin], msft$Open[cwin], mxim$Open[cwin],
                 nflx$Open[cwin], ntap$Open[cwin], nvda$Open[cwin], orcl$Open[cwin],  stx$Open[cwin],
                 symc$Open[cwin],  txn$Open[cwin],  wdc$Open[cwin], xlnx$Open[cwin], yhoo$Open[cwin])
	
	## calculate value of portfolio on date d	
	d <- min(cwin)
    val <-    a$Open[d] + aapl$Open[d] + adbe$Open[d] + amat$Open[d] +  amd$Open[d] +
           amzn$Open[d] + brcd$Open[d] + csco$Open[d] + ebay$Open[d] +  emc$Open[d] +
           goog$Open[d] +  ibm$Open[d] + intc$Open[d] + intu$Open[d] + jnpr$Open[d] +    
           klac$Open[d] +  lsi$Open[d] + mrvl$Open[d] + msft$Open[d] + mxim$Open[d] +
           nflx$Open[d] + ntap$Open[d] + nvda$Open[d] + orcl$Open[d] +  stx$Open[d] +
           symc$Open[d] +  txn$Open[d] +  wdc$Open[d] + xlnx$Open[d] + yhoo$Open[d]
    portval <- append(portval, val)

	## create correlation matrix and zero-out the upper triangle
	tm_cors <- cor(tm)
	m <- matrix(nrow=nrow(tm_cors), ncol=ncol(tm_cors))
	m[upper.tri(m)] <- 0
	diag(m) <- 0
	m[lower.tri(m)] <- 1
	tm_cors <- m * tm_cors

	## mean cor of portfolio
	meancor <- mean(tm_cors[lower.tri(tm_cors)])
	meancor_list <- append(meancor_list, meancor)
	
	## set plot layout 2 on top 1 underneith
	#layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

	# if (drawplots == 1) 
	## set layout 2 by 2
	par(mfrow=c(2,2))

	## density plot with rug
	d <- density(tm_cors[lower.tri(tm_cors)])
	plot(d, xlim=c(-1,1), ylim=c(0,1.5))
	polygon(d, col="darkgreen")
	rug(tm_cors[lower.tri(tm_cors)], col="darkblue")
	
	## wireframe surface plot
	persp(tm_cors, zlab="cor", zlim=c(-1,1),
              main=paste("today -", sd - dicw,
                         "\nmean cor: ", format(round(meancor, 4), nsmall=4),
                         "over", dicw, "days"),
              theta=30, phi=30, expand=0.1, shade=0.5, col="green")

	## build linear model for portfolio value 
    if (length(portval) > 1) {
        l <- lm(portval~seq(1, length(portval)))
	
		## plot portfolio value to date
    	plot(portval, ylim=c(min(portval), max(portval)),
             main=paste("portfolio value to date"),
             xlab=paste("slope: ", round(l$coeff[[2]], 2)),
             type="l", lwd="2", col="darkgreen")
	
		## plot regression line
		abline(l)
	}

	## build sequence for portfolio value within correlation window 
	plotwin <- c(seq(length(portval) - dicw, length(portval)))
	
	## build linear model over correlation window
    if (length(portval) >= length(cwin)) {
        l <- lm(portval[plotwin]~seq(1,dicw+1))

		slopelist <- append(slopelist, l$coeff[[2]])
    
		## plot portfolio value over correlation window
		if (length(portval) >= dicw) {
			plot(portval[plotwin],
                 ylim=c(min(portval[plotwin]), max(portval[plotwin])),
                 main=paste("portfolio value over past", dicw, "days"),
                 xlab=paste("slope: ", round(l$coeff[[2]], 2)),
                 type="l", lwd="2", col="darkgreen")
		}
	
	
		## plot regression line
		abline(l)
	}

	## decrement start day
	sd <- sd - 1
	
	## re-calc correlation window	
	cwin <- sd:(sd - dicw)
	
	print(paste("cwin: ", cwin[1], "-", cwin[dicw]))
}

print("done")
dev.off()
print("graphics device closed")

system("rm cormatdensval.avi")
system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo cormatdensval.avi")
system("rm *.jpg")


