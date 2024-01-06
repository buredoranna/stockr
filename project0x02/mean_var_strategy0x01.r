#library(zoo)

## this doesn't actually plot the day to day value of held stocks, so there may be volitility
## this isn't plotted.

readdata <- T
builddata <- T
plotdata <- T
datapath <- "sp500/"
datapath <- "tech/"

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
start_ndays_ago <- 500

## number of values to use in mean/var calculations
nday_meanvar_window <- 5

## trade every n days
trade_every_ndays <- 11

################################
## read all files in datapath ##
################################
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

## "start_index" is the index in the data frame of the first day for the trading strategy to start
## it is dependent upon how many days in the past you want to start the simulation aka the number
## of days you want to run the simulation over
##
## EXAMPLE:
##          Date  Open  High   Low Close  Volume AdjCLose
##  1 2003-01-02 18.22 19.19 18.14 19.14 3381100    12.55
##  2 2003-01-03 19.00 19.44 18.82 19.05 2622500    12.49
##  3 2003-01-06 19.00 20.11 19.00 19.96 5549500    13.09
##  ...
##  2997 2014-11-25 42.33 42.87 42.31 42.71 2713900    42.71
##  2998 2014-11-26 42.68 42.87 42.53 42.74 1752700    42.74
##  2999 2014-11-28 42.74 42.99 42.59 42.74 1091800    42.74
##  
##  start_ndays_ago = 100
##  start_index = 2999 - 100 = 2899
##  
##  as.Date(mydata[[1]][2899,]$Date) == 2014-07-09
##  
start_index <- length(mydata[[1]]$Open) - start_ndays_ago

## "last_index" is the index of the last day in the stock data frame
last_index <- length(mydata[[1]]$Open)

## this provides a list of indexes which correspond to trading days
trade_day_index_list <- seq(start_index, last_index, trade_every_ndays)

## storage for the stocks that are being held
held_stocks <- data.frame()

## storage for value and percent deltas of portfolio
portfolio_value_df <- data.frame()
portfolio_percent_deltas_df <- data.frame()

## list of transactions
transaction_counter <- 1
xaction_log <- data.frame()

## running percent deltas between trading days
## between date1 and date2 portfolio delta was 1.7%
## between date2 and date3 portfolio delta was -0.6%
running_portfolio_percent_deltas <- c(0)
total_delta <- 0
daily_portfolio_percent_deltas <- c(0)
cumulative_portfolio_percent_delta <- 0
daily_portfolio_percent_delta_list <- c()

## I had to introduce this value because my accounting was wrong... I was saying that if my 
## portfolio lost 75% of its value, then lost another 75%, I had lost a total of 150%.. which is
## wrong. It should have been:
## current_value = current_value + current_value * (percent delta)
## current_value = current_value * (1 + percent_delta)
## EXAMPLE:
##    gains 10% repeatedly
## current_value = 1 * (1 + 0.1) = 1.1
## current_value = 1.1 * (1 + 0.1) = 1.21
## current_value = 1.21 * (1 + 0.1) = 1.331
##
##    gain 10% then looses 7% then looses 2% then gains 6%
## current_value = 1 * (1 + 0.1) = 1.1
## current_value = 1.1 * (1 + (-0.02)) = 1.078
## current_value = 1.078 * (1 + 0.06) = 1.14268

portfolio_coefficient_daily <- 1
portfolio_coefficient_daily_list <- c(1)

portfolio_coefficient_tradeday <- 1
portfolio_coefficient_tradeday_list <- c(1)

tradeday_coefficient_list <- c(1)

## start start_ndays_ago days ago
## implement buying strategy, and keep track of portfolio value every day so I can get a good
## gauge on trading strategy variance
for (dayn in start_index:last_index) {
	todays_date <- as.Date(mydata[[1]]$Date[dayn])

	## if today is a trading day calculate mean/var for each stock on the given trading day
	## and rebalance portfolio
	if (any(trade_day_index_list == dayn)) {
		trade_day_date <- todays_date
		#trading_days <- append(trading_days, as.Date(temp))
		
		cat(paste("trade day: ", as.Date(trade_day_date), "\n"))

		meanvar_df <- data.frame()
		for (stock in 1:length(stock_symbols)) {
			## build index list for mean/var calculation
			right_index <- dayn - 1
			left_index  <- dayn - nday_meanvar_window
			
			meanvar_indexes <- left_index:right_index
		
			## calculate mean/var over indexes
			stock_mean <- mean(mydata[[stock]]$Open[meanvar_indexes])
			stock_var  <-  var(mydata[[stock]]$Open[meanvar_indexes])

			## get value of stock on trading day
			stock_value <- mydata[[stock]]$Open[dayn]

			## build set of values
			stock_meanvar_set <- data.frame(stock,
                                            stock_symbols[stock],
                                            stock_mean,
                                            stock_var,
                                            stock_value,
                                            dayn,
                                            as.Date(mydata[[stock]]$Date[dayn]))
                                  

			meanvar_df <- rbind(meanvar_df, stock_meanvar_set)
		} 

		colnames(meanvar_df) <- c("stock_n", "symbol", "mean", "var", "value", "date_index", "date")

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
	}
	
	## if the length of held_stocks is zero, it is the first time buying stocks
	## else, calculate portfolio percent delta
	if (length(held_stocks) == 0) {
		held_stocks <- buy_stocks
		
        for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "Open"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)

        }
		
	} else if (any(trade_day_index_list == dayn)) {
		
		# date buy/sell stock_n symbol value
        for (x in 1:(length(held_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "s"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "Open"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)
		}

		# date buy/sell stock_n symbol value
        for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- buy_stocks[x, "stock_n"]
			xaction_stock_symbol <- buy_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "Open"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)
        }

                                   
		## if today is a trading day, then rebalance portfolio
		## calculate the percent change of portfolio
		held_value <- sum(held_stocks$value)
		sold_value <- 0
		for (stock in 1:length(held_stocks$stock_n)) {
	    #	print(stock)
			stock_value <- mydata[[held_stocks[stock, "stock_n"]]][dayn, "Open"]
			
			## debugode
			#print(paste("dayn:", dayn))
			#print(paste("stock_n:", held_stocks[stock, "stock_n"],
            #            "symbol:", stock_symbols[held_stocks[stock, "stock_n"]], 
            #            "date:", as.Date(mydata[[held_stocks[stock, "stock_n"]]][dayn, "Date"]),
            #            "value:", stock_value))
			
			sold_value <- sold_value + stock_value
		}
		
		percent_delta <- (sold_value - held_value) / held_value

		## build running list of deltas
		# total_delta <- total_delta + percent_delta
		# running_portfolio_percent_deltas <- append(running_portfolio_percent_deltas, total_delta)

		portfolio_coefficient_tradeday <- portfolio_coefficient_tradeday * (1 + percent_delta)

		## debugode
		#print(paste("coeff:", portfolio_coefficient_tradeday))
		#print(paste("sold_value:", sold_value,
        #            "held_value:", held_value,
        #            "percent_delta:", percent_delta))
		#print(held_stocks)
		#print(buy_stocks)

		portfolio_coefficient_tradeday_list <- append(portfolio_coefficient_tradeday_list,
                                                      portfolio_coefficient_tradeday)

		## display progress
		#cat(paste("bought value: ", held_value,
        #          " sold value: ", sold_value,
        #          " percent delta: ", round(percent_delta * 100, 2), "%\n", sep=""))

		## swap out old stocks for new stocks
		held_stocks <- buy_stocks	
		
		#xaction_log[[transaction_counter]] <- held_stocks
		#transaction_counter <- transaction_counter + 1
		
	}
	

	# tradeday_coefficient_list <- append(tradeday_coefficient_list, portfolio_coefficient_tradeday)

	## keep track of portflio value every day
	todays_portfolio_value <- 0
	for (stock in held_stocks$stock_n) {
		todays_portfolio_value <- todays_portfolio_value + mydata[[stock]]$Open[dayn]
	}

	## portfolio value data frame	
	#portfolio_value <- data.frame(todays_date, todays_portfolio_value)
	#portfolio_value_df <- rbind(portfolio_value_df, portfolio_value)
	
	## daily portfolio value percent deltas
	portfolio_percent_delta <- (todays_portfolio_value / sum(held_stocks$value) - 1)
	portfolio_percent_deltas_df <- rbind(portfolio_percent_deltas_df,
                                         data.frame(todays_date, portfolio_percent_delta))
	
	#cumulative_portfolio_percent_delta <- cumulative_portfolio_percent_delta + portfolio_percent_delta
	#daily_portfolio_percent_delta_list <- append(daily_portfolio_percent_delta_list,
    #                                             cumulative_portfolio_percent_delta)

	portfolio_coefficient_daily <- portfolio_coefficient_daily * (1 + portfolio_percent_delta)
	portfolio_coefficient_daily_list <- append(portfolio_coefficient_daily_list,
                                               portfolio_coefficient_daily)
	# trade_day_index <- trade_day_index + trade_every_ndays
	#daily_portfolio_percent_deltas <- 

}

# index_fund_value_on_trading_day <- append(index_fund_values, sp500_df$Open[trade_day_index_list])

index_fund_percents <- (sp500_df$Open[trade_day_index_list] / sp500_df$Open[start_index]) - 1

outfile <- paste("foo", nday_meanvar_window, ".jpg", sep="")

if (plotdata == T) {
	#xrange <- c(0, max(length(portfolio_delta_percents),length(index_fund_percents)))
	#yrange <- c(-1, max(summary(portfolio_delta_percents)[6], summary(index_fund_percents)[[6]]))
	xrange <- c(0, length(trade_day_index_list) + 1)
	yrange <-  c(-1, 3)

	plot(portfolio_coefficient_tradeday_list - 1, col="darkgreen", xlim=xrange, ylim=yrange, type="l")
	par(new=T)
	plot(index_fund_percents, col="darkblue", xlim=xrange, ylim=yrange, type="l",
         main=paste(nday_meanvar_window, "days in mean/var window\n",
                    "trade every", trade_every_ndays, "days"))
}


#print(xaction_log)
