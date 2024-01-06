require(zoo)
require(corrplot)

## functions I've written 
source("functions.R")


#################################################
## these variables are needed for functionality #
#################################################

load("sp500_25day_roll.Rdata")
#load("sp500_plus_bonds_25day_roll.Rdata")
rolled_stock_data_obj <- sp500_25day_roll
nday_roll_window      <- rolled_stock_data_obj[[1]]$nday_roll_window
stock_symbols         <- rolled_stock_data_obj[[1]]$stock_symbols
roll_list             <- rolled_stock_data_obj[[2]]
stock_number_seq      <- 1:length(stock_symbols)
ndays_of_stock_data   <- length(rolled_stock_data_obj[[2]][[1]]$Date)
datapath              <- rolled_stock_data_obj[[1]]$datapath

## portfolio weight strategy
## "equal", "value"
portfolio_weight_strategy <- "equal"

## number of stocks to hold in portfolio
subdivide_until_less_than_n_stocks <- T
n_or_fewer_stocks_in_quadrant      <- 15
hold_n_stocks                      <- 3

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
years <- round(runif(1, 1, 8), 2)
years <- 5
start_ndays_ago <- round(250*years)

stop_n_years_ago <- 0
stop_n_days_ago  <- round(250*stop_n_years_ago)

## trade every n days
trade_every_ndays <- sample(5:20, 1)
trade_every_ndays <- 60

## number of days in roll window
index_fund_name    <- "spy"
index_fund_df      <- read_index_fund_fn(index_fund_name)
index_fund_risk_df <- add_rolled_variance_to_stock_df(index_fund_df, trade_every_ndays) 

one_year_risk_free_rate     <- 0.0025
viz_correlation_matrix      <- F ## use "corrplot" to visualize correlation matrix
plot_meanvars               <- F
animate_selection_algorithm <- F ## provide visualization of stock selection algorithm
hard_copy                   <- F ## if true, output performance graph to jpeg
plotdata                    <- T
index_fund_trade_day_delta_ratios <- c()

if (animate_selection_algorithm == T) {
    jpeg("tempviz/img%000005d.jpg", width=600, height=600, quality=100)
}

portfolio_returns <- list()

## "start_index" is the index in the data frame of the first day for the
## trading strategy to start
## it is dependent upon how many days in the past you want to start
## the simulation aka the number
## of days you want to run the simulation over
##
## the "start_index" is offset by one because if I want to start 10 days
## ago, that means I want
## the subet of data to include 10 days, but if I say 10 days, I get 11
##
## example:
##  start_n_days_ago == 10
##  total number of days of data = 3000
##  start index = 3000 - 10 = 2990
## 
##         +--- start_n_days_ago
##         |
##        10    9    8    7    6    5    4    3    2    1                                      
## 2990 2991 2992 2993 2994 2995 2996 2997 2998 2999 3000
##    1    2    3    4    5    6    7    8    9   10   11
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
start_index <- ndays_of_stock_data - start_ndays_ago + 1

## "last_index" is the index of the last day in the stock data frame
last_index <- ndays_of_stock_data - stop_n_days_ago

## this provides a list of indexes which correspond to trading days
trade_day_index_seq <- seq(start_index, last_index, trade_every_ndays)

## storage for the stocks that are being held
held_stocks <- data.frame()

## storage for value and percent deltas of portfolio
#portfolio_value_df <- data.frame()
portfolio_percent_deltas_df <- data.frame()

## list of transactions
xaction_log <- data.frame()

## if my portfolio was up 1% then down 1.1% then up 1.21%
## this vector would contain 0.1, -0.11, 0.0121
portfolio_percent_deltas_tradeday <- c()

## I had to introduce this value because my accounting was wrong...
## I was saying that if my 
## portfolio lost 75% of its value, then lost another 75%, I had lost a
## total of 150%.. which is
## wrong. It should have been:
##
## current_value = current_value + current_value * (percent delta)
## current_value = current_value * (1 + percent_delta)
##
## EXAMPLE:
##    gains 10% repeatedly
## current_value = 1    * (1 + 0.1) = 1.1
## current_value = 1.1  * (1 + 0.1) = 1.21
## current_value = 1.21 * (1 + 0.1) = 1.331
##
##    gain 10% then looses 7% then looses 2% then gains 6%
## current_value = 1     * (1 + 0.1) = 1.1
## current_value = 1.1   * (1 + (-0.02)) = 1.078
## current_value = 1.078 * (1 + 0.06) = 1.14268

portfolio_coefficient_tradeday      <- 1
portfolio_coefficient_tradeday_list <- c(1)

## net index fund return
index_fund_return   <- (index_fund_df[start_index:last_index, "AdjClose"] /
                        index_fund_df[start_index, "AdjClose"]) - 1

index_fund_return_df <- data.frame(index_fund_df[start_index:last_index, ],
                                   index_return = index_fund_return)

## log of portfolio betas over window of ownership
portfolio_beta_log <- c()

## start start_ndays_ago days ago
## implement buying strategy, and keep track of portfolio value every day so I can get a good
## gauge on trading strategy variance

#for (dayn in start_index:last_index) {
for (dayn in trade_day_index_seq) { 
	todays_date <- as.Date(roll_list[[1]]$Date[dayn])

	## if today is a trading day calculate mean/var for each stock on the given trading day
	## and rebalance portfolio
	if (any(trade_day_index_seq == dayn)) {
		trade_day_date <- todays_date
		#trading_days <- append(trading_days, as.Date(temp))
		
		cat(paste("trade day: ", as.Date(trade_day_date), "\n"))

		## build data frame of all stocks with mean and variances within nday_roll_window
		meanvar_df <- gen_meanvar_df_new(roll_list, dayn)

		## indexes to use for mean/var analysis
		## the "-1" here is to include data from the past, and to exclude data from
		## the tradeday date.
		## 
		##           +---- trade day
        ##           |
		## 1 2 3 4 5 6
		## |-------|
		##         +--- indexes to use in analyses
		meanvar_indexes <- ((dayn - nday_roll_window + 1):dayn) - 1
	
        #####################	
		## stock selection ##
        #####################
		## to visualize what this is doing set
		## "animate_selection_algorithm" to TRUE or T
		
		###### mean / variance analysis
		median_mean <- median(meanvar_df$mean)
		median_var  <- median(meanvar_df$var)
	
		## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
		}
		
        ## subdivide the stock mean-variance space, based on the median mean and median variance
        ## selecting stocks in the upper left quadrant
		subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                        meanvar_df$var  <= median_var, ]

		## continue sub-dividing mean/var space until we have a list of n_or_fewer stocks
		if(subdivide_until_less_than_n_stocks == T) {
		  while (length(subset_meanvar_df$stock_n) > n_or_fewer_stocks_in_quadrant) {
		  	## debugode
		  	n_stocks_in_subset <- length(subset_meanvar_df$stock_n)
		  	#cat(paste("n_stocks_in_subset: ", n_stocks_in_subset, "\n"))
		  	#readline("press the any key to continue...")
		  	
		  	median_mean <- median(subset_meanvar_df$mean)
		  	median_var  <- median(subset_meanvar_df$var)

            subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var  <= median_var, ]
		  	
		  	## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
		  	  viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
	        }
		  }
		}
			
		## if we have fewer than three stocks, decrease the median_mean to include more stocks
		## s = stock, xaxis = mean, yaxis = variance
		##
		## ..s.|....
		## .s..|....
		## ----+---- --> median_mean
		## ..s.|....
		## s...|....
        ## ....|....
		##      
		## 
		## ..s.|....
		## .s..|....
		## ..s.|....
		## ----+---- --> "new" median_mean 
		## s...|....
        ## ....|....
		##    
		##    
		## 
				
		while (length(subset_meanvar_df$stock_n) < hold_n_stocks) {
			#cat(paste("debug: subset_meanvar_df$stock_n < ", hold_n_stocks, "\n", sep=""))
			median_mean <- median_mean - 0.1
			subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var  <= median_var, ]

            ## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
				viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
	        }

		}
		
		#############################
		## build correlation matrix #
		#############################

		## determine which column number holds the stock's AdjClose values
		adjclose_column <- which(colnames(rolled_stock_data_obj[[2]][[1]]) == "AdjClose")

        ## get value of stocks in upper-left quadrant of mean/var space
		stock_price_df <- as.data.frame(sapply(rolled_stock_data_obj[[2]][subset_meanvar_df$stock_n],
                                               "[",
                                               meanvar_indexes, adjclose_column))
		colnames(stock_price_df) <- subset_meanvar_df$symbol
		cor_stock_price_df       <- cor(stock_price_df)

		## pick stocks whose correlations combine to the shortest distances from the origin in N-space
        pick_list <- pick_n_shortest_distance_to_origin(cor_stock_price_df, hold_n_stocks)
        picks     <- pick_list$chosen_set
	
		############################################
		## visualize correlation matrix and picks ##
		############################################
		if (viz_correlation_matrix == T) {
			corrplot(cor_stock_price_df,
	                 method="color",
	                 type="lower",
	                 main=paste("\ntrade day: ", as.Date(trade_day_date)))
			nrows <- length(rownames(cor_stock_price_df))
			pairs <- combn(picks,2)
			for (pair_n in 1:length(pairs[1,])) {
				x = which(colnames(cor_stock_price_df)      == pairs[1, pair_n])
				y = which(rev(rownames(cor_stock_price_df)) == pairs[2, pair_n])
				segments(x, y, x, y, lwd=10)
			}
			print(picks)
			readline("press the any key to continue...")
		}
			
		buy_stocks <- data.frame()
		for (n in 1:length(picks)) {
          stock_n <- which(stock_symbols == picks[n])
          stock_df <- data.frame(Date    = rolled_stock_data_obj[[2]][[stock_n]][dayn, "Date"],
                                 stock_n = stock_n,
                                 symbol  = picks[n],
                                 value   = rolled_stock_data_obj[[2]][[stock_n]][dayn, "AdjClose"],
                                 mean    = rolled_stock_data_obj[[2]][[stock_n]][dayn, "rollmean"],
                                 var     = rolled_stock_data_obj[[2]][[stock_n]][dayn, "rollvar"])
          buy_stocks <- rbind(buy_stocks, stock_df)
        }

        ## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
			par(new=T)
			viz_buy_stocks(buy_stocks$var, buy_stocks$mean)
			
			## debugode
			#cat(paste("median_var = ", median_var, "\n"))
			#cat(paste("median_mean = ", median_mean, "\n"))
			#cat(paste("buy_stocks = \n"))
			#print(buy_stocks)
			#readline("press the any key to continue...")
			
		}
		
	}
	
	## if the length of held_stocks is zero, it is the first time buying stocks
	## else, sell held stocks, and buy new stocks
	if (length(held_stocks) == 0) {
		held_stocks <- buy_stocks
		
        for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date           <- as.Date(roll_list[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type           <- "b"
			xaction_stock_number   <- held_stocks[x, "stock_n"]
			xaction_stock_symbol   <- held_stocks[x, "symbol"]
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"] ## 
			xaction_stock_close    <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(date     = xaction_date,
                                  xaction  = xaction_type,
                                  stock_n  = xaction_stock_number,
                                  symbol   = xaction_stock_symbol,
                                  AdjClose = xaction_stock_adjclose,
                                  Close    = xaction_stock_close)
            

            xaction_log <- rbind(xaction_log, xaction)
        }
	} else if (any(trade_day_index_seq == dayn)) {
		## record transaction 
        for (x in 1:(length(held_stocks$stock_n))) {
			xaction_date           <- as.Date(roll_list[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type           <- "s"
			xaction_stock_number   <- held_stocks[x, "stock_n"]
			xaction_stock_symbol   <- held_stocks[x, "symbol"]
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"] 
			xaction_stock_close    <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(date     = xaction_date,
                                  xaction  = xaction_type,
                                  stock_n  = xaction_stock_number,
                                  symbol   = xaction_stock_symbol,
                                  AdjClose = xaction_stock_adjclose,
                                  Close    = xaction_stock_close)
            
            xaction_log <- rbind(xaction_log, xaction)
		}
 
 		#######################
		## portfolio returns ##
		#######################
		portfolio_first_day     <- (dayn - trade_every_ndays) + 1
		portfolio_owned_indexes <- portfolio_first_day:dayn
		portfolio_symbols       <- as.character(held_stocks$symbol)

		## equal weighted portfolio ##
		if (portfolio_weight_strategy == "equal") {
			symbol_weight     <- 1 / length(portfolio_symbols)
			portfolio_weights <- c(rep(symbol_weight, length(portfolio_symbols)))
 		}
	
		## weight portfolio by value ##	
		if (portfolio_weight_strategy == "value") {
			portfolio_weights <- get_weights_by_value(rolled_stock_data_obj,
                                                      held_stocks$symbol,
                                                      portfolio_first_day)
 		}
                                                  
		portfolio_return <- get_portfolio_return(rolled_stock_data_obj,
                                                 portfolio_symbols,
                                                 portfolio_weights,
                                                 portfolio_owned_indexes)
		
        portfolio_returns <- append(portfolio_returns, list(portfolio_return))
                                                 

        ## record in transaction log bought stocks
		for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date           <- as.Date(roll_list[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type           <- "b"
			xaction_stock_number   <- buy_stocks[x, "stock_n"]
			xaction_stock_symbol   <- buy_stocks[x, "symbol"]
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"]
			xaction_stock_close    <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(date     = xaction_date,
                                  xaction  = xaction_type,
                                  stock_n  = xaction_stock_number,
                                  symbol   = xaction_stock_symbol,
                                  AdjClose = xaction_stock_adjclose,
                                  Close    = xaction_stock_close)
            
			#colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "AdjClose", "Close")
            xaction_log <- rbind(xaction_log, xaction)
        }

		## if today is a trading day, then calculate the percent change in value of portfolio
		#held_value <- sum(held_stocks$value)
		#sold_value <- 0
		#for (stock in 1:length(held_stocks$stock_n)) {
		#	stock_value <- roll_list[[held_stocks[stock, "stock_n"]]][dayn, "AdjClose"] 
		#	sold_value  <- sold_value + stock_value
		#}
		
		#percent_delta <- (sold_value - held_value) / held_value

		## (portfolio_coefficient_tradeday - 1) is the percent delta to date of my portfolio value

		## portfolio_coefficient_tradeday is the number by which to multiply against my initial 
		## investment to get the value of that investment to date
		#portfolio_coefficient_tradeday <- portfolio_coefficient_tradeday * (1 + percent_delta)

		## debugode
		#print(paste("coeff:", portfolio_coefficient_tradeday))
		#print(paste("sold_value:", sold_value,
        #            "held_value:", held_value,
        #            "percent_delta:", percent_delta))
		#print(held_stocks)
		#print(buy_stocks)

		#portfolio_coefficient_tradeday_list <- append(portfolio_coefficient_tradeday_list,
        #                                              portfolio_coefficient_tradeday)

		held_stocks <- buy_stocks	
	}

	## vizualize 
	if (any(trade_day_index_seq == dayn & plot_meanvars == T)) {
		mean_range <- c(0, 100)
		var_range  <- c(0, 1)
		plot(meanvar_df$mean, meanvar_df$var, xlim=mean_range, ylim=var_range, pch="")
		text(meanvar_df$mean, meanvar_df$var, labels=meanvar_df$symbol)
		#abline(v=median(meanvar_df$mean))
		#abline(h=median(meanvar_df$var))
	}
}

if (plot_meanvars == T || animate_selection_algorithm == T) {
	dev.off()
}

## this prevents an off by one error
## I don't want to include the values of the index fund on the 1st element in trade_day_index_seq
indexes.to.use <- tail(trade_day_index_seq, length(trade_day_index_seq) - 1)
first.index    <- head(indexes.to.use, 1)
index_fund_percents <- (index_fund_df$AdjClose[indexes.to.use] /
                        index_fund_df$AdjClose[first.index]) - 1

## build list of percent deltas of index fund for each trade day
index_fund_trade_day_delta_ratios <- c()
#for (trade_day_index in trade_day_index_seq) {
for (trade_day in 1:length(trade_day_index_seq)) {
	## if this is the first trade day, then the percent delta is zero
	if (trade_day == 1) {
		prcnt_delta_on_trade_day <- 0
	} else {
	## list of indexes
		## value by which to divide other values to get percent changes
		val1 <- index_fund_df$AdjClose[trade_day_index_seq[trade_day - 1]]
		val2 <- index_fund_df$AdjClose[trade_day_index_seq[trade_day]]
	
		## percent changes from first index to last index
		#val_prcnts <- (index_fund_df$AdjClose[indexes] / val1) - 1
		#prcnt_delta_on_trade_day <- tail(val_prcnts,1)
		prcnt_delta_on_trade_day <- (val2 - val1) / val1
		
	}

	index_fund_trade_day_delta_ratios <- append(index_fund_trade_day_delta_ratios,
                                                prcnt_delta_on_trade_day)
}


if (plotdata == T) {
	if (hard_copy == T) {
		outfile <- paste("tempviz/",
                         years, "yr", "_",
                         nday_roll_window, "day-win_",
                         trade_every_ndays, "day-trade_",
                         selection_dominance, "-dom", ".jpg", sep="")
		jpeg(outfile, height=600, width=600, quality=100)
	}

	gross_return <- 1
	portfolio_return_df <- data.frame(Date = numeric(0),
		                              return = numeric(0))
	for (n in 1:length(portfolio_returns)) {
        net_return   <- tail(portfolio_returns[[n]]$return, 1)
        date         <- tail(portfolio_returns[[n]]$Date, 1)
		
        ## this is the gross return determined by multiplying the gross return from each
        ## portfolio return, so net returns for a portfolio is 3%, 1.7%, 0.23% then the calculated
        ## gross return would be (1 + 0.03) * (1 + 0.017) * (1 + 0.0023) = 1.049919
        ## resulting in a total of aprox 5%
        gross_return <- gross_return * (net_return + 1)
		return_df    <- data.frame(Date         = date,
	                               net_return   = net_return,
		                           gross_return = gross_return)
		
		portfolio_return_df <- rbind(portfolio_return_df,  return_df)
	}
    
	xrange <- c(0, length(trade_day_index_seq) + 1)
	yrange <- range(portfolio_coefficient_tradeday_list - 1,
                    index_fund_percents,
                    portfolio_return_df$gross_return - 1)
		
	plot(portfolio_return_df$gross_return - 1, xlim=xrange, ylim=yrange, type="l", col="darkgreen")
	par(new=T)
	plot(index_fund_percents, col="darkblue", xlim=xrange, ylim=yrange, type="l",
         main = paste("data path: ", datapath, ", index fund: ", index_fund_name, "\n",
                      nday_roll_window, " day mean/var window\n",
                      "trade every ", trade_every_ndays, " days",
                      " over ", years, " years, "))

	if(hard_copy == T) {
		dev.off()
	}
}

### index fund summary data        
index_summary <- summary(index_fund_trade_day_delta_ratios)
index_stddev  <- sd(index_fund_trade_day_delta_ratios)
index_var     <- var(index_fund_trade_day_delta_ratios)

## the "+1" here is to match the results in index_fund_risk_log which, when calculating
## risk uses the AdjClose of the stock on the day it is purchase as well as on the day it was
## sold, for the variance calculation

###############
### results ###
###############

cat(paste("##### ", years, " years\n", sep=""))
cat(paste("##### trade every ", trade_every_ndays, " days\n", sep=""))
cat(paste("##### hold ", hold_n_stocks, " stocks in portfolio\n", sep=""))
cat(paste("##### low risk annual rate: ", one_year_risk_free_rate * 100, "%\n\n", sep=""))

## index fund ##
average_annual_index_fund_variance <- var(index_fund_percents) / years
average_annual_index_fund_stddev   <- sqrt(average_annual_index_fund_variance)

fund_return                   <- tail(index_fund_percents, 1)
#average_annual_fund_return    <- fund_return / years
average_annual_fund_return    <- fund_return ^ (1 / years)
return_percent                <- round(fund_return * 100, 2)
average_annual_return_percent <- return_percent / years
stddev_percent                <- round(average_annual_index_fund_stddev * 100, 2)
sharpe_ratio                  <- (average_annual_fund_return - one_year_risk_free_rate) /
                                  average_annual_index_fund_stddev 
sharpe_ratio                  <- round(sharpe_ratio, 2)
index_sharpe                  <- sharpe_ratio

cat(paste("### index fund ###\n"))
cat(paste("index fund return     : ", return_percent, "%\n", sep=""))
cat(paste("average annual return : ", round(return_percent / years, 2), "%\n", sep=""))
cat(paste("annual average std dev: ", stddev_percent, "%\n", sep=""))
cat(paste("sharpe ratio          : ", sharpe_ratio, "\n\n", sep=""))


## managed weighted portfolio ##
average_annual_portfolio_variance <- var(portfolio_return_df$gross_return - 1) / years
average_annual_portfolio_stddev   <- sqrt(average_annual_portfolio_variance)

portfolio_return                <- tail(portfolio_return_df$gross_return - 1, 1)
#average_annaul_portfolio_return <- portfolio_return / years
average_annaul_portfolio_return <- portfolio_return ^ (1 / years)
return_percent                  <- round(portfolio_return * 100, 2)
stddev_percent                  <- round(average_annual_portfolio_stddev * 100, 2)
sharpe_ratio                    <- (average_annaul_portfolio_return - one_year_risk_free_rate) /
                                   average_annual_portfolio_stddev
sharpe_ratio                    <- round(sharpe_ratio, 2)

cat(paste("### ", portfolio_weight_strategy, "-weighted portfolio ###\n", sep=""))
cat(paste("return                : ", return_percent, "%\n", sep=""))
cat(paste("average annual return : ", round(return_percent / years, 2), "%\n", sep=""))
cat(paste("average annual std dev: ", stddev_percent, "%\n", sep=""))
cat(paste("sharpe ratio          : ", sharpe_ratio, "\n\n", sep=""))

sharpe_delta <- round((sharpe_ratio - index_sharpe) / index_sharpe, 2) * 100
cat(paste("sharpe delta          : ", sharpe_delta, "%\n", sep=""))



