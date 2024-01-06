## read data 
port <- read.table("/home/user/data/stocks/intraday/20140129.csv", sep=",", header=T)

## scatterplot two stocks with trendlines and correlation
cor_plot <- function(tkr_a, tkr_b, names) {
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

	## build a linear regression model for tkr_a
	l <- lm(tkr_a~c(seq(1:length(tkr_a))))
	
	## plot values for tkr_a, regression line and axis
	plot(tkr_a, main=paste(names[1],"(r) :", names[2], "(b)"), col="red",
         ylim=c(min(tkr_a), min(tkr_a) + y_range_delta), axes=F, ylab="", xlab="")
	par(new=T)
	abline(l, col="red"); par(new=T)
	axis(2, labels=FALSE); par(new=T)
	
	## build linear regression model for tkr_b
	l <- lm(tkr_b~c(seq(1:length(tkr_b))))
	
	## plot values for tkr_b, regression line and axis
	plot(tkr_b, col="blue", ylim=c(min(tkr_b), min(tkr_b) + y_range_delta),
         axes=F, ylab="", xlab=c(paste(round(cor_tkrs_ab*100, 2), "% correlation")))
	par(new=T)
	abline(l, col="blue")
	par(new=T)
	axis(1)
}

