## this calculates the cor matrix of a port portfolio between
## most recent date (today) and n-days into the past

## read intraday portfolio
# port_20140128 <- read.table("/home/user/data/stocks/intraday/20140128.csv", sep=",", header=T)
# port_20140129 <- read.table("/home/user/data/stocks/intraday/20140129.csv", sep=",", header=T)
# port_20140130 <- read.table("/home/user/data/stocks/intraday/20140130.csv", sep=",", header=T)
# port_20140131 <- read.table("/home/user/data/stocks/intraday/20140131.csv", sep=",", header=T)
# port_20140203 <- read.table("/home/user/data/stocks/intraday/20140203.csv", sep=",", header=T)
port_20140206 <- read.table("/home/user/data/stocks/intraday/20140206.csv", sep=",", header=T)

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
	
	## build a linear model for tkr_a
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt~c(seq(1:length(tkr_a_prcnt))))
	slope_a <- coef(lm.tkr_a_prcnt)[[2]]
	
	## plot values of tkr_a, regression line and axis
	plot(tkr_a_prcnt,
         main=paste(names[1],"(b) :", names[2], "(r)", names[3], "(g)"),
         col="blue", ylim=yrange, axes=F, xlab="", ylab=paste("% of val at t=0"))
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
	axis(1)
	par(new=T)

	## build linear model for market
	lm.tkr_m_prcnt <- lm(tkr_m_prcnt~c(seq(1:length(tkr_m_prcnt))))
	slope_m <- coef(lm.tkr_m_prcnt)[[2]]
	
	## grab slopes
	slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                coef(lm.tkr_b_prcnt)[[2]],
                coef(lm.tkr_m_prcnt)[[2]])
	maxslope <- which.max(slopes)
	
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
	plot(tkr_prcnt, main=name, ylab="% of open", xlab="sample n")

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

## generate animation of two tkrs correlation over windows worth of data
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

## this strategy holds the security which has the greater slope over 
## the number of days in the plot window 
strat0x01 <- function(tkr_a, tkr_b, tkr_m, ndays, names) {
	
	## build window for linear model
	win <- length(tkr_a):(length(tkr_a) - ndays)
	
	## map tkr values as percent of tkr value at t=0
	tkr_a_prcnt <- ((tkr_a / tkr_a[1]) - 1.0) * 100
	tkr_b_prcnt <- ((tkr_b / tkr_b[1]) - 1.0) * 100
	tkr_m_prcnt <- ((tkr_m / tkr_m[1]) - 1.0) * 100

	## build linear models
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt~c(1:ndays))
	lm.tkr_b_prcnt <- lm(tkr_b_prcnt~c(1:ndays))
	lm.tkr_m_prcnt <- lm(tkr_m_prcnt~c(1:ndays))

	## grab slopes
	slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                coef(lm.tkr_b_prcnt)[[2]],
                coef(lm.tkr_m_prcnt)[[2]])
	maxslope <- which.max(slopes)
	
	info <- paste("max slope:", names[maxslope])
}

