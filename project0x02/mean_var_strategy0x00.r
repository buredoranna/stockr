#library(zoo)

## this should also be compared to a portfolio where I just select some stocks I like, and hold them,
## as well as an index fund
readdata <- T
builddata <- T
plotdata <- T
datapath <- "tech/"

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
start_ndays_ago <- 365*5.1

## number of values to use in mean/var calculations
nday_meanvar_window <- 5

## trade every n days
trade_every_ndays <- 1

################################
## read all files in datapath ##
################################

#sp500_index
if (readdata == T) {
	mydata <- list()
	stock_symbols <- list.files(datapath)
	for (f in 1:length(stock_symbols)) {
		fullpath <- paste(datapath, stock_symbols[f], sep="")
		print(fullpath)
		mydata[[f]] <- read.csv(fullpath, header=T)
	}
	sp500_df<- read.csv("indexfund/gspc", header=T)
}

start_index <- length(mydata[[1]]$Open) - start_ndays_ago
trade_day_index <- start_index

held_stocks <- data.frame()

portfolio_value <- c()
portfolio_percent_deltas <- c()
cumulative_percent_delta <- c()
running_delta <- c()
total_delta <- 0
trading_days <- c()
index_fund_values <- c()

## start trading strategy and continue until there aren't enough days left
while (trade_day_index <= length(mydata[[1]]$Open)) {

	temp <- mydata[[1]]$Date[trade_day_index]
	trading_days <- append(trading_days, as.Date(temp))
	
	#cat(paste("trade day:", temp, "\n"))

	## calculate mean/var for each stock on the given trading day
	meanvar_df <- data.frame()
	for (stock in 1:length(stock_symbols)) {
		## build index list for mean/var calculation
		right_index <- trade_day_index - 1
		left_index  <- trade_day_index - nday_meanvar_window 
		meanvar_indexes <- left_index:right_index
	
		## calculate mean/var over indexes
		stock_mean <- mean(mydata[[stock]]$Open[meanvar_indexes])
		stock_var  <-  var(mydata[[stock]]$Open[meanvar_indexes])

		
		## get value of stock on trading day
		stock_value <- mydata[[stock]]$Open[trade_day_index]

		## build set of values
		stock_meanvar_set <- c(stock, stock_mean, stock_var, stock_value)

		meanvar_df <- rbind(meanvar_df, stock_meanvar_set)
	}

	colnames(meanvar_df) <- c("stock_n", "mean", "var", "value")

	## order by mean
	foo <- meanvar_df[order(meanvar_df$mean, decreasing=T), ]
	
	## choose top 10 
	bar <- head(foo, 10)
	
	## order by variance
	baz <- bar[order(bar$var), ]
	
	## choose top 3
	buy_stocks <- head(baz, 3)
	
	## cleanup
	rm(foo, bar, baz)

	## if the length of held_stocks is zero, it is the first time buying stocks
	## else, calculate portfolio percent delta
	if (length(held_stocks) == 0) {
		held_stocks <- buy_stocks
	} else {
		## calculate the percent change of portfolio
		held_value <- sum(held_stocks$value)
		sold_value <- sum(buy_stocks$value)
		percent_delta <- (sold_value - held_value) / held_value
		
		## build running list of deltas
		total_delta <- total_delta + percent_delta
		running_delta <- append(running_delta, total_delta)

		## display progress
		#cat(paste("bought value: ", held_value,
        #          " sold value: ", sold_value,
        #          " percent delta: ", round(percent_delta * 100, 2), "%\n", sep=""))

		## swap out old stocks for new stocks
		held_stocks <- buy_stocks	
	}

	index_fund_values <- append(index_fund_values, sp500_df$Open[trade_day_index])
	trade_day_index <- trade_day_index + trade_every_ndays
}

#cat(paste("\nfirst trading day: ", trading_days[1], 
#          "last trading day : ", trading_days[length(trading_days)], 
#          "percent change   : ", round(running_delta[length(running_delta)] * 100, 2), "%\n", sep=""))
#

cat(paste(trading_days[1], " ", trading_days[length(trading_days)], " ", 
          round(running_delta[length(running_delta)] * 100, 2), "%\n", sep=""))
	
index_fund_percents <- (index_fund_values / index_fund_values[1]) - 1

cat("summary strategy\n")
print(summary(running_delta))
print(var(running_delta))
cat("summary sp500\n")
print(summary(index_fund_percents))
cat(paste("index fund final:", round(tail(index_fund_percents, 1) * 100, 2), "%\n", sep=""))
print(var(index_fund_percents))

if (plotdata == T) {
	xrange <- c(0, max(length(running_delta),length(index_fund_percents)))
	yrange <- c(-1, max(summary(running_delta)[6], summary(index_fund_percents)[[6]]))
	plot(running_delta, xlim=xrange, ylim=yrange, col="darkgreen", lwd=2, type="l")
	par(new=T)
	plot(index_fund_percents, xlim=xrange, ylim=yrange, col="darkblue", lwd=2, type="l")
}
