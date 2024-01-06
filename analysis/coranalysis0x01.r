## this calculates the cor matrix of a port portfolio between
## most recent date (today) and n-days into the past

## read intraday portfolio
#port <- read.table("/home/user/data/stocks/intraday/20140131.csv", sep=",", header=T)

#l <- length(port_intraday[1,])
#tkrs <- colnames(port_intraday[1, 2:l])

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read historical portfolio
#print("reading data...")
#source("readstocks.r")
#print("done")

# open device for jpg output
#if (jpegs == 1) {
#	print("generating images...")
#	system("rm *.jpg")
#	jpeg("img%00004d.jpg", width=800, height=700, quality=100)
#}

cor_over_win <- function(tkr_a, tkr_b, ndaysago) {
	return(cor(tkr_a[1:ndaysago], tkr_b[1:ndaysago]))
}

## scatterplot two stocks with trendlines and correlation
cor_plot_tkrs_ab <- function(tkr_a, tkr_b, names) {
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

get_min_cors <- function(intraday_port) {
	rl <- length(intraday_port[1,])
	cor_matrix <- cor(intraday_port[,2:rl])
	
	result <- t(sapply(seq(nrow(cor_matrix)), function(i) {
	j <- which.min(cor_matrix[i,])
	c(paste(rownames(cor_matrix)[i], colnames(cor_matrix)[j], sep='/'), cor_matrix[i,j])
	}))
	return(result)
}

#ndays <- 20
#par(mfrow=c(1,2))
#ymin <- min(c(aapl$Open[ndays:1], port_intraday$aapl))
#ymax <- max(c(aapl$Open[ndays:1], port_intraday$aapl))
#yrange <- c(ymin, ymax)

#plot(aapl$Open[ndays:1], ylim=yrange, axes=F, ylab="", xlab="", type="l")
#regline <- lm(aapl$Open[ndays:1]~seq(1,ndays))
#abline(regline)
#abline(h=500)
#axis(1, at=c(seq(0,ndays,2)), labels=seq(-ndays,0,2))
#axis(2)
#plot(port_intraday$aapl, ylim=yrange, axes=F, ylab="", xlab="")
#abline(h=500)
#axis(2)


