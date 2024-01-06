## this calculates the cor matrix of a port portfolio between
## most recent date (today) and n-days into the past

## read intraday portfolio
# port_20140128 <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)
# port_20140129 <- read.table("/home/user/data/stocks/intraday/20140129.csv", sep=",", header=T)
# port_20140130 <- read.table("/home/user/data/stocks/intraday/20140130.csv", sep=",", header=T)
# port_20140131 <- read.table("/home/user/data/stocks/intraday/20140131.csv", sep=",", header=T)
# port_20140203 <- read.table("/home/user/data/stocks/intraday/20140203.csv", sep=",", header=T)
# port_20140206 <- read.table("/home/user/data/stocks/intraday/20140206.csv", sep=",", header=T)

#plot(density(cor.port_20140128[lower.tri(cor.port_20140128)]), main="port cor 20140128")
#plot(density(cor.port_20140129[lower.tri(cor.port_20140129)]), main="port cor 20140129")
#plot(density(cor.port_20140130[lower.tri(cor.port_20140130)]), main="port cor 20140130")
#plot(density(cor.port_20140131[lower.tri(cor.port_20140131)]), main="port cor 20140131")
#plot(density(cor.port_20140203[lower.tri(cor.port_20140203)]), main="port cor 20140203")

## create tkr list
#l <- length(port[1,])
#tkr_list <- colnames(port[1, 2:l])

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read historical portfolio data
#print("reading data...")
#source("readstocks.r")
#print("done")

## returns correlation of two tkrs over specified n-day window
cor_over_win_tkrs_ab <- function(tkr_a, tkr_b, ndaysago) {
	return(cor(tkr_a[1:ndaysago], tkr_b[1:ndaysago]))
}

## scatterplot two stocks with trendlines and correlation metric
plot_tkrs_ab_cor <- function(tkr_a, tkr_b, names) {
	cor_tkrs_ab <- cor(tkr_a, tkr_b)

	## determine the range to use for the plot of the two stocks
	diff_tkr_a <- ceiling(diff(range(tkr_a)))
	diff_tkr_b <- ceiling(diff(range(tkr_b)))

	## use the ranges of the two stocks to calculate the yrange values
	if (diff_tkr_a > diff_tkr_b) {
		y_range_delta <- diff_tkr_a
	} else {
		y_range_delta <- diff_tkr_b
	}	

	## build a linear model for tkr_a
	l <- lm(tkr_a~c(seq(1:length(tkr_a))))
	
	## plot values for tkr_a, regression line and axis
	plot(tkr_a, main=paste(names[1],"(b) :", names[2], "(r)"), col="blue",
         ylim=c(min(tkr_a), min(tkr_a) + y_range_delta), axes=F, ylab="", xlab="")
	abline(l, col="blue")
	axis(2, col="blue")
	par(new=T)
	
	## build linear model for tkr_b
	l <- lm(tkr_b~c(seq(1:length(tkr_b))))
	
	## plot values for tkr_b, regression line and axis
	plot(tkr_b, col="red", ylim=c(min(tkr_b), min(tkr_b) + y_range_delta),
         axes=F, ylab="", xlab=c(paste(round(cor_tkrs_ab*100, 2), "% correlation")))
	abline(l, col="red")
	axis(4, col="red")
	# par(new=T)
	axis(1)
}

## scatterplot two stocks with trendlines and correlation metric as percentage of open
plot_tkrs_ab_prcnt <- function(tkr_a, tkr_b, names) {
	## generate correlation matrix between two tkrs
	cor_tkrs_ab <- cor(tkr_a, tkr_b)
	
	## build tkr value list as percentage of open
	tkr_a_prcnt <- ((tkr_a / tkr_a[1]) - 1.0) * 100
	tkr_b_prcnt <- ((tkr_b / tkr_b[1]) - 1.0) * 100

	## build yrange
	min.tkr_a_prcnt <- min(tkr_a_prcnt) 
	max.tkr_a_prcnt <- max(tkr_a_prcnt)
	
	min.tkr_b_prcnt <- min(tkr_b_prcnt)
	max.tkr_b_prcnt <- max(tkr_b_prcnt)
	
	if (min.tkr_a_prcnt <= min.tkr_b_prcnt) {
		ymin <- min.tkr_a_prcnt
	} else {
		ymin <- min.tkr_b_prcnt 
	}

	if (max.tkr_a_prcnt >= max.tkr_b_prcnt) {
		ymax <- max.tkr_a_prcnt
	} else {
		ymax <- max.tkr_b_prcnt 
	}
	
	yrange <- c(ymin, ymax)
	
	## build a linear model for tkr_a
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt~c(seq(1:length(tkr_a_prcnt))))
	slope_a <- coef(lm.tkr_a_prcnt)[[2]]
	
	## plot values of tkr_a, regression line and axis
	plot(tkr_a_prcnt, main=paste(names[1],"(b) :", names[2], "(r)"), col="blue",
         ylim=yrange, axes=F, xlab="", ylab=paste("% of open"))
	abline(lm.tkr_a_prcnt, col="blue")
	axis(2, col="blue")
	par(new=T)
	
	## build linear model for tkr_b
	lm.tkr_b_prcnt <- lm(tkr_b_prcnt~c(seq(1:length(tkr_b_prcnt))))
	slope_b <- coef(lm.tkr_b_prcnt)[[2]]

	## slope ratio
	slope_ratio <- round(slope_a / slope_b, 3)
	
	## plot values tkr_b, regression line and axis
	plot(tkr_b_prcnt, col="red", ylim=yrange,
         axes=F, ylab="",
         xlab=c(paste(round(cor_tkrs_ab*100, 2), "% correlation", "\nslope ratio", slope_ratio)))
	abline(lm.tkr_b_prcnt, col="red")
	axis(4, col="red")
	# par(new=T)
	axis(1)
}

## scatterplot two stocks with trendlines and correlation metric as percentage of open
## including a market index
plot_tkrs_abm_prcnt <- function(tkr_a, tkr_b, tkr_m, names) {
	## generate correlation matrix between two tkrs
	cor_tkrs_ab <- cor(tkr_a, tkr_b)
	
	## build tkr values as percentage of value at t=0
	tkr_a_prcnt <- ((tkr_a / tkr_a[1]) - 1.0) * 100
	tkr_b_prcnt <- ((tkr_b / tkr_b[1]) - 1.0) * 100
	tkr_m_prcnt <- ((tkr_m / tkr_m[1]) - 1.0) * 100

	## build yrange
	ymin <- min(c(min(tkr_a_prcnt), min(tkr_b_prcnt), min(tkr_m_prcnt)))
	ymax <- max(c(max(tkr_a_prcnt), max(tkr_b_prcnt), max(tkr_m_prcnt)))
	yrange <- c(ymin, ymax)
	
	## build a linear models for tkr_a
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt~c(seq(1:length(tkr_a_prcnt))))
	lm.tkr_b_prcnt <- lm(tkr_b_prcnt~c(seq(1:length(tkr_b_prcnt))))
	lm.tkr_m_prcnt <- lm(tkr_m_prcnt~c(seq(1:length(tkr_m_prcnt))))
	
	## grab slopes
	slope_a <- coef(lm.tkr_a_prcnt)[[2]]
	slope_b <- coef(lm.tkr_b_prcnt)[[2]]
	slope_m <- coef(lm.tkr_m_prcnt)[[2]]
	
	## grab slopes
	slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                coef(lm.tkr_b_prcnt)[[2]],
                coef(lm.tkr_m_prcnt)[[2]])
	max_slope <- max(slopes)
	max_slope_name <- names[which.max(slopes)]

	## build xlab
	xinfo <- paste("max slope:", max_slope_name, round(max_slope, 2))
		
	## plot values of tkr_a, regression line and axis
	plot(tkr_a_prcnt,
         main = paste(names[1],"(b) :", names[2], "(r)", names[3], "(g)"),
         col  = "blue",
         ylim = yrange,
         axes = F,
         xlab = xinfo,
         ylab = paste("% of val at t=0"))

	abline(lm.tkr_a_prcnt, col="blue")

	axis(2, col="blue")

	par(new=T)
	
	## slope ratio
	slope_ratio <- round(slope_a / slope_b, 3)
	
	## plot values tkr_b, regression line and axis
	plot(tkr_b_prcnt, col="red", ylim=yrange, axes=F, ylab="", xlab="")
	abline(lm.tkr_b_prcnt, col="red")
	axis(4, col="red")
	axis(1)
	par(new=T)
	
	## plot values of tkr_m (market), regression line and axis
	plot(tkr_m_prcnt, col="darkgreen", ylim=yrange, axes=F, xlab="", ylab="")
	abline(lm.tkr_m_prcnt, col="darkgreen")
}

## given an intraday portfolio, generate a list of minimum correlations between tkrs
get_min_cors <- function(intraday_port) {
	rl <- length(intraday_port[1,])
	cor_matrix <- cor(intraday_port[,2:rl])
	
	result <- t(sapply(seq(nrow(cor_matrix)), function(i) {
		j <- which.min(cor_matrix[i,])
		c(paste(rownames(cor_matrix)[i], colnames(cor_matrix)[j], sep='/'), cor_matrix[i,j])
	}))
	return(result)
}

## plot tkr value as percentage of open
plot_tkr_prcnt <- function(tkr, name) {
	tkr_prcnt <- ((tkr / tkr[1]) - 1) * 100
	delta_prcnt <- (tkr[length(tkr)] - tkr[1]) / tkr[1] * 100
	plot(tkr_prcnt, main=paste(name, " ", round(delta_prcnt, 2), "%", sep=""),
                               ylab="% of open", xlab="sample n")

	lm.tkr <- lm(tkr_prcnt~seq(1:length(tkr)))
	abline(lm.tkr)
}

## plot tkr value with trendline
plot_tkr <- function(tkr, name) {
	lm.tkr <- lm(tkr~c(seq(1:length(tkr))))
	
	plot(tkr, main=name, ylab="value", xlab="sample n")
	abline(lm.tkr)
}

## build linear models for each tkr in portfolio and return list of fits
port_sigmas <- function(port) {	
	sigmas <- c()
	nsamples <- length(port[,1])
	for (tkr in 2:length(port)) {
		lm.tkr <- lm(port[,tkr]~c(seq(1:nsamples)))
		s <- summary(lm.tkr)$sigma
		# pair <- c(colnames(port[tkr]), s)
	
		sigmas <- rbind(sigmas, c(colnames(port[tkr]), s))
	}
	return(sigmas)
}


## generate animation of two tkrs correlation over day's worth of data
animate_tkrs_ab <- function(tkr_a, tkr_b, names) {
	## open device for jpg output
	system("rm *.jpg")
	print("generating images...")
	jpeg("img%00004d.jpg", width=600, height=600, quality=100)

	for (t in 2:length(tkr_a)) {	
		win <- 1:t
		plot_tkrs_ab_cor_prcnt(tkr_a[win], tkr_b[win], names)
	}
	print("done")
	dev.off()
	print("graphics device closed")

	## compile video
	system("rm out.avi")
	print("previous out.avi removed")
	print("generating video...")
	system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo out.avi")
	print("video compilation complete")	
	print("cleaning up jpegs")
	system("rm *.jpg")
	print("done")
}

## plot tkr value over time as percentage, trendline and fit info
plot_tkr_prcnt_with_fit_info <- function(tkr, name) {
	## calculate value of tkr as percent of earliest value
	tkr_prcnt <- (tkr / tkr[1] - 1) * 100
	
	## set layout 2 by 2
	# par(mfrow=c(2,1))

	## build linear model
	lm.tkr_prcnt <- lm(tkr_prcnt~c(1:length(tkr_prcnt)))
	lm_resids <- residuals(lm.tkr_prcnt)

	## build density model
	d.lm_resids <- density(lm_resids)

	## display plots
	plot(tkr_prcnt)
	abline(lm.tkr_prcnt)

	## new plot window	
	dev.new()
	plot(d.lm_resids)
	abline(v=mean(lm_resids), lwd="2", col="darkgreen")

	## print info
	cat(paste("residuals -\n  mean:", round(mean(lm_resids), 2)))
	cat(paste("\n  sd:", round(sd(lm_resids), 2)))
        cat(paste("\n% change:", round( ((tkr[length(tkr)] - tkr[1]) / tkr[1]) * 100, 2), "\n"))


	## resids
	return((lm_resids / tkr) * 100)
	
	## histogram of linmod residuals
	#hist(residuals(lm.tkr_prcnt), breaks=50)

	## I want to know how much a todays tkr value differs from the regression line aka "expected value"
	## as a precentage of current tkr value
}

## generate animation of two tkrs and market over ndays
## example:
##    perhaps the tkrs are given 1000 days worth of data, 
##    but only plot 20 days at a time 
##    fn(tkr_a$Open[1000:1], tkr_b$Open[1000:1], tkr_m$Open[1000:1], ndays, ...)
animate_tkrs_abm_win <- function(tkr_a, tkr_b, tkr_m, ndays, names) {
	## open device for jpg output
	system("rm *.jpg")
	print("generating images...")
	jpeg("img%00004d.jpg", width=600, height=600, quality=100)

	## build plot window
	win <- length(tkr_a):(length(tkr_a) - ndays)
	
	## generate images
	while(min(win) >= 0) {
        plot_tkrs_abm_prcnt(tkr_a[win], tkr_b[win], tkr_m[win], names)
		win <- win - 1
	}
	print("done")
	dev.off()
	print("graphics device closed")

	## compile video
	system("rm out.avi")
	print("previous out.avi removed")
	print("generating video...")
	system("avconv -r 5 -i img%00004d.jpg -crf 18 -c:v rawvideo out.avi")
	print("video compilation complete")	
	print("cleaning up jpegs")
	system("rm *.jpg")
	print("done")
}


## returns a data frame of opens going back sd days
build_tkr_df <- function(sd) {
	return_df <- data.frame(aapl$Date[1:sd], 
                            aapl$Open[1:sd],    a$Open[1:sd], adbe$Open[1:sd], amat$Open[1:sd],
                             amd$Open[1:sd], amzn$Open[1:sd], brcd$Open[1:sd], csco$Open[1:sd],
                            ebay$Open[1:sd],  emc$Open[1:sd], goog$Open[1:sd], gspc$Open[1:sd],
                              hp$Open[1:sd],  ibm$Open[1:sd], intc$Open[1:sd], intu$Open[1:sd],
                            jnpr$Open[1:sd], klac$Open[1:sd],  lsi$Open[1:sd], mrvl$Open[1:sd],
                            msft$Open[1:sd], mxim$Open[1:sd], nflx$Open[1:sd], ntap$Open[1:sd],
                            nvda$Open[1:sd], orcl$Open[1:sd], sndk$Open[1:sd],  stx$Open[1:sd],
                            symc$Open[1:sd],  txn$Open[1:sd],  wdc$Open[1:sd], xlnx$Open[1:sd],
                            yhoo$Open[1:sd])

	colnames(return_df) <- c("Date",
                             "aapl",    "a", "adbe", "amat",
                              "amd", "amzn", "brcd", "csco",
                             "ebay",  "emc", "goog", "gspc",
                               "hp",  "ibm", "intc", "intu",
                             "jnpr", "klac",  "lsi", "mrvl",
                             "msft", "mxim", "nflx", "ntap",
                             "nvda", "orcl", "sndk",  "stx",
                             "symc",  "txn",  "wdc", "xlnx",
                             "yhoo")
	return(return_df)
}

## returns the std dev of the residuals of a linear model of 
## the given tkr (over the entire set of values passed)
sd.resids.lm.tkr_prcnt_win <- function(tkr) {
	tkr_prcnt <- tkr$Open / tkr$Open[length(tkr$Open)]
	lm.tkr_prcnt <- lm(tkr_prcnt~c(seq(1:length(tkr_prcnt))))
	return(sd(residuals(lm.tkr_prcnt)))
}

sd.resids_win <- function(win) {
	temp <- data.frame()
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(   a[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(aapl[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(adbe[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(amat[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( amd[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(amzn[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(brcd[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(csco[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(ebay[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( emc[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(goog[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(  hp[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( ibm[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(intc[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(intu[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(jnpr[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(klac[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( lsi[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(mrvl[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(msft[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(mxim[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(nflx[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(ntap[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(nvda[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(orcl[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(sndk[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( stx[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(symc[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( txn[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win( wdc[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(xlnx[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(yhoo[win,]))
	temp <- rbind(temp, sd.resids.lm.tkr_prcnt_win(gspc[win,]))

	colnames(temp) <- c("stddev")
	rownames(temp) <- c("a", "aapl", "adbe", "amat", "amd", "amzn", "brcd", "csco", "ebay",
                        "emc", "goog", "hp", "ibm", "intc", "intu", "jnpr", "klac", "lsi",
                        "mrvl", "msft", "mxim", "nflx", "ntap", "nvda", "orcl", "sndk", "stx",
                        "symc", "txn", "wdc", "xlnx", "yhoo", "gspc")
	return(temp)
}

## returns std dev of percent delta between yesterday's open and today's open
get_tkr_p_vol <- function(tkr_df) {
	sdate <- tkr_df$Date[nrow(tkr_df)]
	edate <- tkr_df$Date[1]
	tkr_opens <- tkr_df$Open
	past <- length(tkr_opens)
	prcnt_delta_list <- c()
	for (yday in past:2) {
		tday <- yday - 1
		prcnt_delta <- ((tkr_opens[tday] - tkr_opens[yday]) / tkr_opens[yday]) * 100
		prcnt_delta_list <- append(prcnt_delta_list, prcnt_delta)
	}
	## debugode
	#return(prcnt_delta_list)
	
	ret_df <- data.frame(sdate,
                         edate,
                         mean(prcnt_delta_list),
                         sd(prcnt_delta_list))
	colnames(ret_df) <- c("sdate",
                          "edate",
                          "mean_prcnt_delta",
                          "sd_prcnt_delta")
	return(ret_df)
}


