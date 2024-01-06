#library(zoo)

## this does a slightly different method of mean / variance analysis
## previously I did "sort by mean, choose top 10, then sort by variance and choose top 3"
##
## this method segments the mean / var space (x-axis = mean, y-axis = variance)
## using the median mean and median variance, and from those four areas, chooses the stocks 
## in the lower right quadrant.
##
## added too this method is an analyis of correlation. so, rather than just the top mean/var
## stocks it is the stock with the top mean and lowest correlation among three stocks

plot_meanvars <- F
animate_selection_algorithm <- F ## provide visualization of stock selection algorithm
hard_copy <- F ## if true, output performance graph to jpeg
selection_dominance <- "mean" ## mean or variance 
readdata <- T
builddata <- T
plotdata <- T
datapath <- "sp500_and_tech/"
datapath <- "tech/"
index_fund_trade_day_prcnt_deltas <- c()

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
years <- 1
start_ndays_ago <- round(365*years) - 0

## number of values to use in mean/var calculations
nday_meanvar_window <- 25

## trade every n days
trade_every_ndays <- 5

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
##  as.Date(mydata[[1]][2899,]$Date) == "2014-07-09"
##  
start_index <- length(mydata[[1]]$AdjClose) - start_ndays_ago

## "last_index" is the index of the last day in the stock data frame
last_index <- length(mydata[[1]]$AdjClose)

## this provides a list of indexes which correspond to trading days
trade_day_index_seq <- seq(start_index, last_index, trade_every_ndays)

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
running_portfolio_percent_deltas <- c()
total_delta <- 0
daily_portfolio_percent_deltas <- c(0)
cumulative_portfolio_percent_delta <- 0
daily_portfolio_percent_delta_list <- c()

## if my portfolio was up 1% then down 1.1% then up 1.21%
## this vector would contain 0.1, -0.11, 0.0121
portfolio_percent_deltas_tradeday <- c()

## I had to introduce this value because my accounting was wrong... I was saying that if my 
## portfolio lost 75% of its value, then lost another 75%, I had lost a total of 150%.. which is
## wrong. It should have been:
##
## current_value = current_value + current_value * (percent delta)
## current_value = current_value * (1 + percent_delta)
##
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

#portfolio_coefficient_daily <- 1
#portfolio_coefficient_daily_list <- c(1)

portfolio_coefficient_tradeday <- 1
portfolio_coefficient_tradeday_list <- c(1)

## this is used for comparison to a "managed" portfolio, subject to "mean/var" analysis and 
## rebalancing. The tech_portfolio is just a collection of tech stocks, held without over time
## with no changes.
if (datapath == "tech/") {
	tech_portfolio_coeff_tradeday <- 1
	tech_portfolio_coeff_tradeday_list <- c(1)
	#held_tech_stock_symbols <- c("amzn", "csco", "ebay", "emc", "msft")
	held_tech_stock_symbols <- stock_symbols
	held_tech_stockn <- c()
	tech_portfolio_percent_value <- 0

	for (s in 1:length(held_tech_stock_symbols)) {
		held_tech_stockn <- append(held_tech_stockn, which(stock_symbols == held_tech_stock_symbols[s]))
	}
	
	for (s in held_tech_stockn) {
		tech_portfolio_percent_value <- tech_portfolio_percent_value +
	                                    mydata[[s]][trade_day_index_seq, "AdjClose"] ## 
	}
	
	tech_portfolio_percent_value <- (tech_portfolio_percent_value / tech_portfolio_percent_value[1]) - 1
}

tradeday_coefficient_list <- c(1)

## start start_ndays_ago days ago
## implement buying strategy, and keep track of portfolio value every day so I can get a good
## gauge on trading strategy variance

## for visuals
if (plot_meanvars == T || animate_selection_algorithm == T) {
	jpeg("tempviz/img%000005d.jpg", width=600, height=600, quality=100)
}
for (dayn in start_index:last_index) {
	todays_date <- as.Date(mydata[[1]]$Date[dayn])

	## if today is a trading day calculate mean/var for each stock on the given trading day
	## and rebalance portfolio
	if (any(trade_day_index_seq == dayn)) {
		trade_day_date <- todays_date
		#trading_days <- append(trading_days, as.Date(temp))
		
		cat(paste("trade day: ", as.Date(trade_day_date), "\n"))

		## build data frame of all stocks with mean and variances within nday_meanvar_window
		meanvar_df <- data.frame()
		for (stock in 1:length(stock_symbols)) {
			## indexes to use for mean/var analysis
			## the "-1" here is to include data from the past, and to exclude data from
			## the tradeday date.
			## 
			##           +---- trade day
            ##           |
			## 1 2 3 4 5 6
			## |-------|
			##         +--- indexes to use in analises
			meanvar_indexes <- ((dayn - nday_meanvar_window):dayn) - 1
		
			## calculate mean/var over indexes
			#stock_mean <- mean(mydata[[stock]]$Open[meanvar_indexes])
			#stock_mean <- mean(unlist(mydata[[stock]][meanvar_indexes, 2:5]))
			stock_mean <- mean(unlist(mydata[[stock]][meanvar_indexes, "AdjClose"]))
			
			#stock_var  <-  var(mydata[[stock]]$Open[meanvar_indexes])
			#stock_var  <-  var(unlist(mydata[[stock]][meanvar_indexes, 2:5]))
			stock_var <- var(unlist(mydata[[stock]][meanvar_indexes, "AdjClose"]))
			#var_vector	

			#stock on trading day
			#stock_value <- mydata[[stock]]$AdjClose[dayn] ## 
			stock_value <- mydata[[stock]][dayn, "AdjClose"] ## 
			#stock_value <- mydata[[stock]][dayn, 7]

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

		######### SELECTION ALGORITHM ######### 
		######### to see what this is doing set "animate_selection_algorithm" to TRUE
		
		###### mean / variance analysis
		median_mean <- median(meanvar_df$mean)
		median_var  <- median(meanvar_df$var)

		## choose three stocks.
		## if three stocks are not in the lower right quadrant
		## divide median_var by 2, until the subset dataframe contains three or more stocks
		## then sort the subset dataframe and choose the highest mean
		subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                        meanvar_df$var <= median_var, ]

		## this gives an upper bound on variance
		while (median_var >= 1) {
			median_var <- median_var / 2
		}

		## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
			abline(v=median_mean)
			abline(h=median_var)
		}

		## continue sub-dividing mean/var space until we have a list of 10 or fewer stocks
		while (length(subset_meanvar_df$stock_n) > 10) {
			median_mean <- median(subset_meanvar_df$mean)
			median_var  <- median(subset_meanvar_df$var)

            subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var <= median_var, ]

			## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
	            plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
	            abline(v=median_mean)
	            abline(h=median_var)
	        }

		}
			
		## if we have fewer than three stocks, decrease the median_mean to include more stocks
		## in the lower right quadrant.
		## s = stock, xaxis = mean, yaxis = variance
		##
		## ....|....
		## ....|....
		## ----+----
		## ...s|..s.
		## ....|.s..
		##     |
		##     +---- median_mean
		##      
		## 
		## ..|......
		## ..|......
		## --+------
		## ..|s...s.
		## ..|...s..
		##   |
		##   +---- median_mean / 2
		## 
				
		while (length(subset_meanvar_df$stock_n) < 3) {
			median_mean <- median_mean / 2
			subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var <= median_var, ]

            ## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
	            plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
	            abline(v=median_mean)
	            abline(h=median_var)
	        }

		}
		
		if (selection_dominance == "mean")  {
			## order subset my mean
			subset_meanvar_df <- subset_meanvar_df[order(subset_meanvar_df$mean, decreasing=T),]
		} else {
			## order subset by variance
			subset_meanvar_df <- subset_meanvar_df[order(subset_meanvar_df$var), ]
		}
		
		## instead of buying the top three stocks
		# buy_stocks <- head(subset_meanvar_df, 3)
		
		## first buy the stock with the highest mean
		## then buy two more stocks with the lowest correlation with the first one

		## build correlation matrix
		stock_price_df <- data.frame()
		for (stock_n in subset_meanvar_df$stock_n) {
		    data_column <- as.vector(mydata[[stock_n]][meanvar_indexes, "AdjClose"])
		    if (which(subset_meanvar_df$stock_n == stock_n) == 1)  {
		        stock_price_df <- data_column
		    } else {
		        data_column <- as.vector(mydata[[stock_n]][meanvar_indexes, "AdjClose"])
		        stock_price_df <- cbind(stock_price_df, data_column)
		    } 
		}
		colnames(stock_price_df) <- subset_meanvar_df$symbol
		cor_stock_price_df <- cor(stock_price_df)

		## based on the correlation matrix, and buying the first stock with the highest mean
		## buy two more stocks with the lowest correlation with the first
		buy_stocks <- data.frame()
		stock1 <- as.character(subset_meanvar_df[order(subset_meanvar_df$mean), ][1, "symbol"])
		temp <- abs(cor_stock_price_df)[stock1, ]
		temp <- temp[order(temp)]
		stock2 <- names(temp)[1]
		stock3 <- names(temp)[2]
		stock1_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock1, ]
		stock2_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock2, ]
		stock3_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock3, ]
		buy_stocks <- rbind(buy_stocks, stock1_df, stock2_df, stock3_df)
	
        ## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
			abline(v=median_mean)
			abline(h=median_var)
			par(new=T)
			plot(buy_stocks$mean, buy_stocks$var, xlim=c(0,100), ylim=c(0,1), pch=20, col="green")
		}
		
	}
	
	## if the length of held_stocks is zero, it is the first time buying stocks
	## else, sell held stocks, and buy new stocks
	if (length(held_stocks) == 0) {
		held_stocks <- buy_stocks
		
        for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "AdjClose"] ## 

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)
        }
	} else if (any(trade_day_index_seq == dayn)) {
		## record in transaction log sold stock 
        for (x in 1:(length(held_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "s"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "AdjClose"] ## 

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)
		}

        ## record in transaction log bought stocks
		for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date <- as.Date(mydata[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- buy_stocks[x, "stock_n"]
			xaction_stock_symbol <- buy_stocks[x, "symbol"]
			xaction_stock_value <- mydata[[xaction_stock_number]][dayn, "AdjClose"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_value)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "value")
            xaction_log <- rbind(xaction_log, xaction)
        }

		## if today is a trading day, then calculate the percent change in value of portfolio
		held_value <- sum(held_stocks$value)
		sold_value <- 0
		for (stock in 1:length(held_stocks$stock_n)) {
			stock_value <- mydata[[held_stocks[stock, "stock_n"]]][dayn, "AdjClose"] ## 
			
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

		## a list containing portfolio percent delta on trade day
		#running_portfolio_percent_deltas <- append(running_portfolio_percent_deltas, percent_delta)

		## a list of percent changes of portfolio
		portfolio_percent_deltas_tradeday <- append(portfolio_percent_deltas_tradeday, percent_delta)

		## list of percent changes of index fund
		#index_fund_prcnt_delta_on_trade_day	


		## (portfolio_coefficient_tradeday - 1) is the percent delta to date of my portfolio value
		## portfolio_coefficient_tradeday is the number by which to multiply against my initial 
		## investment to get the value of that investment to date
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
		#todays_portfolio_value <- todays_portfolio_value + mydata[[stock]]$AdjClose[dayn]
		todays_portfolio_value <- todays_portfolio_value + mydata[[stock]][dayn, "AdjClose"]
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

	#portfolio_coefficient_daily <- portfolio_coefficient_daily * (1 + portfolio_percent_delta)
	#portfolio_coefficient_daily_list <- append(portfolio_coefficient_daily_list,
    #                                           portfolio_coefficient_daily)
	# trade_day_index <- trade_day_index + trade_every_ndays
	#daily_portfolio_percent_deltas <- 


	## vizualize 
	if (any(trade_day_index_seq == dayn & plot_meanvars == T)) {
		mean_range <- c(0, 100)
		var_range  <- c(0, 1)
		plot(meanvar_df$mean, meanvar_df$var, xlim=mean_range, ylim=var_range, pch="")
		text(meanvar_df$mean, meanvar_df$var, labels=meanvar_df$symbol)
		abline(v=median(meanvar_df$mean))
		abline(h=median(meanvar_df$var))
	}
	
	

}

if (plot_meanvars == T || animate_selection_algorithm == T) {
	dev.off()
}

#index_fund_value_on_trading_day <- append(index_fund_values, sp500_df$Open[trade_day_index_seq])

index_fund_percents <- (sp500_df$AdjClose[trade_day_index_seq] / sp500_df$AdjClose[start_index]) - 1

## build list of percent deltas of index fund for each trade day
index_fund_trade_day_prcnt_deltas <- c()
#for (trade_day_index in trade_day_index_seq) {
for (trade_day in 1:length(trade_day_index_seq)) {
	## if this is the first trade day, then the percent delta is zero
	if (trade_day == 1) {
		prcnt_delta_on_trade_day <- 0
	} else {
	## list of indexes
		## value by which to divide other values to get percent changes
		val1 <- sp500_df$AdjClose[trade_day_index_seq[trade_day - 1]]
		val2 <- sp500_df$AdjClose[trade_day_index_seq[trade_day]]
	
		## percent changes from first index to last index
		#val_prcnts <- (sp500_df$AdjClose[indexes] / val1) - 1
		#prcnt_delta_on_trade_day <- tail(val_prcnts,1)
		prcnt_delta_on_trade_day <- (val2 - val1) / val1
	}

	index_fund_trade_day_prcnt_deltas <- append(index_fund_trade_day_prcnt_deltas,
                                                prcnt_delta_on_trade_day)
}


if (plotdata == T) {
	if (hard_copy == T) {
		outfile <- paste("viz/",
                         years, "yr", "_",
                         nday_meanvar_window, "day-win_",
                         trade_every_ndays, "day-trade_",
                         selection_dominance, "-dom", ".jpg", sep="")
		jpeg(outfile, height=600, width=600, quality=100)
	}
	
	xrange <- c(0, length(trade_day_index_seq) + 1)

	if (datapath == "tech/") {
		yrange_min <- min(tech_portfolio_percent_value,
                         (portfolio_coefficient_tradeday_list - 1),
                          index_fund_percents)
	
		yrange_min <- floor(yrange_min)

		yrange_max <- max(tech_portfolio_percent_value,
                     (portfolio_coefficient_tradeday_list - 1),
                      index_fund_percents)
		yrange_max <- ceiling(yrange_max)
		yrange <-  c(yrange_min, yrange_max)
	} else {
        yrange_min <- min((portfolio_coefficient_tradeday_list - 1),
                          index_fund_percents)

        yrange_min <- floor(yrange_min)

        yrange_max <- max((portfolio_coefficient_tradeday_list - 1),
                      index_fund_percents)
        yrange_max <- ceiling(yrange_max)
        yrange <-  c(yrange_min, yrange_max)
	}


	if (datapath == "tech/") {
		plot(tech_portfolio_percent_value, col="darkred", xlim=xrange, ylim=yrange, type="l")
		par(new=T)
	}
	
	plot(portfolio_coefficient_tradeday_list - 1, col="darkgreen", xlim=xrange, ylim=yrange, type="l")
	par(new=T)
	plot(index_fund_percents, col="darkblue", xlim=xrange, ylim=yrange, type="l",
        main=paste(nday_meanvar_window, "day mean/var window\n",
                    "trade every", trade_every_ndays, "days\n",
                    "over", years, "years, ",
                    selection_dominance, "dominant"))
	if(hard_copy == T) {
		dev.off()
	}
}

cat(paste("###### ", years, " year(s) ", datapath, " trading history ######\n", sep=""))
cat(paste("meanvar window: ", nday_meanvar_window, " ", sep=""))
cat(paste("trade every: ", trade_every_ndays, "\n\n", sep=""))
#cat(paste("trade period mean: ",
#          mean(portfolio_percent_deltas_df$portfolio_percent_delta), "\n", sep=""))
#
#cat(paste("tarde period variance: ",
#          var(portfolio_percent_deltas_df$portfolio_percent_delta), "\n", sep=""))
#


#prcnt_ret <- round((tail(portfolio_coefficient_tradeday_list,1) - 1) * 100)
prcnt_ret <- round((portfolio_coefficient_tradeday - 1) * 100, 2)
cat(paste("managed return: ", prcnt_ret, "%\n", sep=""))

## portfolio_percent_deltas_df is daily data, as apposed to
#managed_summary_daily <- summary(portfolio_percent_deltas_df$portfolio_percent_delta)
#cat(paste("managed summary daily:\n"))
#print(round(managed_summary, 3))
cat(paste("managed summary tradeday: portfolio_percent_deltas_tradeday\n"))
managed_summary_tradeday <- summary(portfolio_percent_deltas_tradeday)
print(round(managed_summary_tradeday, 3))
        
cat(paste("\n\nindex return: ", round(tail(index_fund_percents,1) * 100, 1), "%\n", sep=""))
index_summary <- summary(index_fund_trade_day_prcnt_deltas)
cat("index summary tradeday: index_fund_trade_day_prcnt_deltas \n")
print(round(index_summary, 3))
cat("\n")


#x11()

#plot(hist(portfolio_percent_deltas_df$portfolio_percent_delta, breaks=50))
#abline(v=mean(portfolio_percent_deltas_df$portfolio_percent_delta))




##print(xaction_log)
