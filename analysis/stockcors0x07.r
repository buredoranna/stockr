# this calculations various attributes of a portfolio
# and if desired, creates plots 

foo <- c()

sd <- 2000 # number days into the past to start
dicw <- 20  # days in correlation window
cwin <- sd:(sd - dicw) # window for correlation calculation

## 0-DO NOT plot, 1-DO plot
plot_portcor <- 0 
plot_boxplot <- 0
plot_portval <- 0
plot_portvalcorw <- 0 
totalplots <- plot_portcor + plot_portval + plot_portvalcorw + plot_boxplot
drawplots <- 0 # quad plot (legacy)

# 0-DO NOT output jpegs, 1-DO output jpegs
if (totalplots > 0) {
	jpegs <- 1 
} else {
	jpegs <- 0
}

portval <- c() # storage for portfolio value
corlist <- c() # storage for correlations
meancor <- c() # storage for mean correlation
meancor_list <- c()
slopelist <- c()
firstopens <- c() # storage for portfolio opening values for first date in data frame
curopens <- c() # storage for current portfolio opening values 
deltaopens <- matrix() # return on investment to date
deltaopens_list<- c()
appl_delta <- c()

minportcor_list <- c()
maxportcor_list <- c()

tkrs <- c(   "a", "aapl", "adbe", "amat",  "amd",
          "amzn", "brcd", "csco", "ebay",  "emc",
          "goog",  "ibm", "intc", "intu", "jnpr",
          "klac",  "lsi", "mrvl", "msft", "mxim",
          "nflx", "ntap", "nvda", "orcl",  "stx",
          "symc",  "txn",  "wdc", "xlnx", "yhoo")

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read in the data sets
#print("reading data...")
#source("readstocks.r")
#print("done")

# open device for jpg output
if (jpegs == 1) {
	print("generating images...")
	system("rm *.jpg")
	jpeg("img%00004d.jpg", width=800, height=700, quality=100)
}

## build list of opening values for earliest date in tables (last year is earlier than this year)
d <- sd
firstopens <- cbind(    a$Open[d], aapl$Open[d], adbe$Open[d], amat$Open[d],  amd$Open[d],
                     amzn$Open[d], brcd$Open[d], csco$Open[d], ebay$Open[d],  emc$Open[d],
                     goog$Open[d],  ibm$Open[d], intc$Open[d], intu$Open[d], jnpr$Open[d],
                     klac$Open[d],  lsi$Open[d], mrvl$Open[d], msft$Open[d], mxim$Open[d],
                     nflx$Open[d], ntap$Open[d], nvda$Open[d], orcl$Open[d],  stx$Open[d],
                     symc$Open[d],  txn$Open[d],  wdc$Open[d], xlnx$Open[d], yhoo$Open[d])

curopens <- firstopens

while (sd - dicw >= 1) {
	# set current date 
	d <- min(cwin)

	## create list of current value opens
	curopens <- cbind(    a$Open[d], aapl$Open[d], adbe$Open[d], amat$Open[d],  amd$Open[d],
                       amzn$Open[d], brcd$Open[d], csco$Open[d], ebay$Open[d],  emc$Open[d],
                       goog$Open[d],  ibm$Open[d], intc$Open[d], intu$Open[d], jnpr$Open[d],
                       klac$Open[d],  lsi$Open[d], mrvl$Open[d], msft$Open[d], mxim$Open[d],
                       nflx$Open[d], ntap$Open[d], nvda$Open[d], orcl$Open[d],  stx$Open[d],
                       symc$Open[d],  txn$Open[d],  wdc$Open[d], xlnx$Open[d], yhoo$Open[d])
	deltaopens <- curopens - firstopens
	aapl_delta <- append(appl_delta, deltaopens[2])
	foo <- append(foo, curopens[2] / firstopens[2] * 100 )
	
	## bind together a matrix of ticker opening values for correlation calculation
	tm <- cbind(    a$Open[cwin], aapl$Open[cwin], adbe$Open[cwin], amat$Open[cwin],  amd$Open[cwin],
                 amzn$Open[cwin], brcd$Open[cwin], csco$Open[cwin], ebay$Open[cwin],  emc$Open[cwin],
                 goog$Open[cwin],  ibm$Open[cwin], intc$Open[cwin], intu$Open[cwin], jnpr$Open[cwin],
                 klac$Open[cwin],  lsi$Open[cwin], mrvl$Open[cwin], msft$Open[cwin], mxim$Open[cwin],
                 nflx$Open[cwin], ntap$Open[cwin], nvda$Open[cwin], orcl$Open[cwin],  stx$Open[cwin],
                 symc$Open[cwin],  txn$Open[cwin],  wdc$Open[cwin], xlnx$Open[cwin], yhoo$Open[cwin])
	colnames(tm) <- tkrs
	rownames(tm) <- a$Date[cwin]
	
	## calculate value of portfolio on date d	
    val <-    a$Open[d] + aapl$Open[d] + adbe$Open[d] + amat$Open[d] +  amd$Open[d] +
           amzn$Open[d] + brcd$Open[d] + csco$Open[d] + ebay$Open[d] +  emc$Open[d] +
           goog$Open[d] +  ibm$Open[d] + intc$Open[d] + intu$Open[d] + jnpr$Open[d] +    
           klac$Open[d] +  lsi$Open[d] + mrvl$Open[d] + msft$Open[d] + mxim$Open[d] +
           nflx$Open[d] + ntap$Open[d] + nvda$Open[d] + orcl$Open[d] +  stx$Open[d] +
           symc$Open[d] +  txn$Open[d] +  wdc$Open[d] + xlnx$Open[d] + yhoo$Open[d]
    portval <- append(portval, val)

	## create correlation matrix and zero-out the duplicate upper triangle
	tm_cors <- cor(tm)
	m <- matrix(nrow=nrow(tm_cors), ncol=ncol(tm_cors))
	m[upper.tri(m)] <- 0
	diag(m) <- 0
	m[lower.tri(m)] <- 1
	tm_cors <- m * tm_cors

	## mean cor of portfolio
	meancor <- mean(tm_cors[lower.tri(tm_cors)])
	meancor_list <- append(meancor_list, meancor)
	
	## minimum portfolio correlation within cwin-days
	minportcor <- min(tm_cors[lower.tri(tm_cors)])
	minportcor_list <- append(minportcor_list, minportcor)
	maxportcor <- max(tm_cors[lower.tri(tm_cors)])
	maxportcor_list <- append(maxportcor_list, maxportcor)
	# print(paste("portfolio cor: [", round(minportcor,2), ",", round(maxportcor,2), "]" ))
	# hist(tm_cors[lower.tri(tm_cors)], breaks=c(seq(-1,1,.01)), col="darkgreen")
	#print(paste("mean cor:", meancor, "min cor:", minportcor, "difference:",  )
	
	## set layout row-col
	if (totalplots > 1) {
		par(mfrow=c(1,2))
	}

	## line plot of portfolio correlation
	plotwin <- (length(meancor_list) - dicw):length(meancor_list)
	if (plot_portcor == 1) {
		# plot max, min, mean portfolio variance
		if (length(meancor_list) > dicw) {
			# plotwin <- 1:length(meancor_list)  
			plot(maxportcor_list[plotwin],
                 ylab="cor",
                 xlab=paste("mean(min)", round(mean(minportcor_list[plotwin]), 4),
                            "mean(max)", round(mean(maxportcor_list[plotwin]), 4),
                            "mean(mean)", round(mean(meancor_list[plotwin]), 4)),
                 ylim=c(-1,1), type="l", lwd="2", col="darkred")
			par(new=T)
			plot(meancor_list[plotwin], xlab="", ylab="",
                 ylim=c(-1,1), type="l", lwd="2", col="darkgreen")
			par(new=T)
			plot(minportcor_list[plotwin], xlab="", ylab="",
                 ylim=c(-1,1), type="l", lwd="2", col="darkblue") 
			
			if (plot_boxplot == 1) {
				boxplot(meancor_list[plotwin])
			}
		}
	}
	
	## build linear model of port value over correlation window
	plotwin <- c(seq(length(portval) - dicw, length(portval)))
    if (length(portval) >= length(cwin)) {
       	l <- lm(portval[plotwin]~seq(1,dicw+1))
		slopelist <- append(slopelist, l$coeff[[2]])
	}
   
	## plot portfolio value over correlation window
	if (plot_portvalcorw == 1) { 
		if (length(portval) >= dicw) {
			## build portfolio value window 
			
			## plot port value over window
			plot(portval[plotwin],
                 ylim=c(min(portval[plotwin]), max(portval[plotwin])),
                 main=paste("portfolio value over past", dicw, "days"),
                 xlab=paste("slope: ", round(l$coeff[[2]], 2)),
                 type="l", lwd="2", col="darkgreen")
		
		## plot regression line
		abline(l)
		}
	}

	## density plot with rug
	if (drawplots == 1) {
		d <- density(tm_cors[lower.tri(tm_cors)])
		# plot(d, xlim=c(-1,1), ylim=c(0,1.5))
		plot(d, xlim=c(1,-1), ylim=c(0,1.5))
		polygon(d, col="darkgreen")
		rug(tm_cors[lower.tri(tm_cors)], col="darkblue")
	}
	
	## wireframe surface plot
	if (drawplots == 1) {
		persp(tm_cors, zlab="cor", zlim=c(-1,1),
              main=paste("today -", sd - dicw,
                         "\nmean cor: ", format(round(meancor, 4), nsmall=4),
                         "over", dicw, "days"),
              theta=30, phi=30, expand=0.1, shade=0.5, col="green")
	}

	## build linear model for portfolio value to date
    if (length(portval) > 1) {
        l <- lm(portval~seq(1, length(portval)))
	}
	
	## plot portfolio value to date
	if (plot_portval == 1) {
    	plot(portval, ylim=c(min(portval), max(portval)),
             main=paste("portfolio value to date"),
             xlab=paste("slope: ", round(l$coeff[[2]], 2)),
             type="l", lwd="2", col="darkgreen")
	
		## plot regression line
		abline(l)
	}
	
	## decrement start day
	sd <- sd - 1
	
	## re-calc correlation window	
	cwin <- sd:(sd - dicw)
	
	if (drawplots == 1) {	
		print(paste("cwin: ", cwin[1], "-", cwin[dicw]))
	}
}

if (jpegs == 1) {
	dev.off()
	print("done")
	print("graphics device closed")

	system("rm out.avi")
	system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo out.avi")
	system("rm *.jpg")
}


