library(zoo)
readdata <- T
builddata <- T
plotdata <- T
datapath <- "sp500/"

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
lookatpastndays <- 2000

## number of events to use in rolling calculations
rollwindow <- 60

## ranges for plotting
mean_range <- c(0, 200)
var_range  <- c(0, 20)

################################
## read all files in datapath ##
################################

mydata <- list()
if (readdata == T) {
	stock_symbols <- list.files(datapath)
	for (f in 1:length(stock_symbols)) {
		fullpath <- paste(datapath, stock_symbols[f], sep="")
		print(fullpath)
		mydata[[f]] <- read.csv(fullpath, header=T)
	}
}


#####################################################################################
## calculate rolling means / variances based on the number of days in lookpastndays #
#####################################################################################

## allocate storage for results
if (builddata == T) {
	rollmeans <- list()
	rollvars  <- list()
	for (stock in 1:length(mydata)) {
		cat(paste("rolling:", stock_symbols[stock], "\n"))
		if (length(mydata[[stock]]$Open) >= lookatpastndays) {
			left  <- length(mydata[[stock]]$Open) - lookatpastndays + 1
			right <- length(mydata[[stock]]$Open)
			rollmeans[[stock]] <- rollmean(mydata[[stock]]$Open[left:right], rollwindow)
			rollvars[[stock]]  <- rollapply(mydata[[stock]]$Open[left:right], rollwindow, FUN=var)
		} else {
			rollmeans[[stock]] <- -1
			rollvars[[stock]]  <- -1
		}
	}
}

#################################################################################
## build list of data frames containing "mean / var" data points for each stock #
#################################################################################

if (builddata == T) {
	
	## storage for results
	meanvar_sets<- list()
	
	## iterate over all stock symbols
	for (stock in 1:length(stock_symbols)) {
		foo <- data.frame()
		cat(paste("building mean-var data frames:", stock, stock_symbols[stock], "\n"))
		
		## iterate over all values in rolled sets
		for (dayn in 1:length(rollmeans[[stock]])) {
			data_point <- c(rollmeans[[stock]][dayn], rollvars[[stock]][dayn])
			foo <- rbind(foo, data_point)
		}
		colnames(foo) <- c("mean", "var")
		meanvar_sets[[stock]] <- foo
	}
}
	

#################
## viz results ##
#################

if (plotdata == T) {
	jpeg("viz/img%000005d.jpg", width=600, height=600, quality=100)
	
	## iterate over each day in rolled sets
	total_days <- length(meanvar_sets[[1]]$mean)
	#for (dayn in 1:length(meanvar_sets[[1]]$mean)) {
	for (dayn in 1:total_days) {
	cat(paste("plotting image... "))
		## temp storage for plot sets
		plot_set <- data.frame()
		
		## iterate over each stock symbol
		for (stock in 1:length(stock_symbols)) {
			plot_set <- rbind(plot_set, meanvar_sets[[stock]][dayn, ])
		}
	
		colnames(plot_set) <- c("mean", "var")
		#plot(plot_set, xlim=mean_range, ylim=var_range, pch=".")
		#text(plot_set, labels=stock_symbols)
		plot(plot_set, xlim=mean_range, ylim=var_range, pch=20)
		cat(paste(dayn, "of", total_days, "\n"))
	}
	dev.off()
}
		
	
