require(zoo)

## functions I've written 
source("functions.R")

#################################################
## these variables are needed for functionality #
#################################################

#load("sp500_25day_roll.Rdata")
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
hold_n_stocks <- 5

## number of days in roll window
index_fund_name <- "vti"

bond.fund.df <- read.csv("lowriskpool/lqd", head=T)

one_year_risk_free_rate <- 0.0025

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
years <- round(runif(1, 1, 8), 2)
years <- 3

start_ndays_ago <- round(365*years)

subdivide_until_less_than_n_stocks <- T
n_or_fewer_stocks_in_quadrant <- 13

## number of values to use in mean/var calculations
#nday_roll_window <- nday_roll_window

## trade every n days
trade_every_ndays <- sample(5:20, 1)
trade_every_ndays <- 5

#readdata <- F
#rolldata <- F ## if F, rolling mean/var is calculated on the fly, otherwise it is rolled

plot_meanvars <- F
animate_selection_algorithm <- F ## provide visualization of stock selection algorithm
hard_copy <- F ## if true, output performance graph to jpeg
plotdata <- T
index_fund_trade_day_delta_ratios <- c()

#index_fund_df      <- read_index_fund_fn(index_fund_name)
index_fund_df <- read.csv("indexfund/tacs/vti_quantquote", header=T)
index_fund_risk_df <- add_rolled_variance_to_stock_df(index_fund_df, trade_every_ndays) 

if (animate_selection_algorithm == T) {
    jpeg("tempviz/img%000005d.jpg", width=600, height=600, quality=100)
}

portfolio_returns <- list()

## "start_index" is the index in the data frame of the first day for the trading strategy to start
## it is dependent upon how many days in the past you want to start the simulation aka the number
## of days you want to run the simulation over
##
## the "start_index" is offset by one because if I want to start 10 days ago, that means I want
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
last_index <- ndays_of_stock_data

## this provides a list of indexes which correspond to trading days
trade_day_index_seq <- seq(start_index, last_index, trade_every_ndays)

## storage for the stocks that are being held
held_stocks <- data.frame()

## storage for value and percent deltas of portfolio
#portfolio_value_df <- data.frame()
portfolio_percent_deltas_df <- data.frame()

## list of transactions
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
## current_value = 1    * (1 + 0.1) = 1.1
## current_value = 1.1  * (1 + 0.1) = 1.21
## current_value = 1.21 * (1 + 0.1) = 1.331
##
##    gain 10% then looses 7% then looses 2% then gains 6%
## current_value = 1     * (1 + 0.1) = 1.1
## current_value = 1.1   * (1 + (-0.02)) = 1.078
## current_value = 1.078 * (1 + 0.06) = 1.14268

#portfolio_coefficient_daily <- 1
#portfolio_coefficient_daily_list <- c(1)

portfolio_coefficient_tradeday <- 1
portfolio_coefficient_tradeday_list <- c(1)

tradeday_coefficient_list <- c(1)

## a vector of distances from origin of chosen stocks in 3-space
pick_returns <- list()
distances <- c()
correlation_tripplets <- c()
temp_tripplets <- data.frame()

## every trade day, the risk of the portfolio over the past trade_every_n_days days is calculated
## and saved in the risk_log
risk_log <- c()
risk_log_xaction_list <- list()

## index fund risk
#index_fund_risk_log <- c()
index_fund_risk_log <- index_fund_risk_df$rollvar[trade_day_index_seq]
index_fund_return <- (index_fund_df[start_index:last_index, "Close"] /
                     index_fund_df[start_index, "Close"]) - 1

index_fund_return_df <- data.frame(index_fund_df[start_index:last_index, ],
                                   index_return = index_fund_return)

## log of portfolio betas over window of ownership
portfolio_beta_log <- c()

## start start_ndays_ago days ago
## implement buying strategy, and keep track of portfolio value every day so I can get a good
## gauge on trading strategy variance

#for (dayn in start_index:last_index) {
for (dayn in trade_day_index_seq) { 
	todays_date <- roll_list[[1]]$Date[dayn]

	## if today is a trading day calculate mean/var for each stock on the given trading day
	## and rebalance portfolio
	if (any(trade_day_index_seq == dayn)) {
		trade_day_date <- todays_date
		#trading_days <- append(trading_days, as.Date(temp))
		
		cat(paste("trade day: ", trade_day_date, "\n"))

		## build data frame of all stocks with mean and variances within nday_roll_window

		meanvar_df <- data.frame()
		#meanvar_df <- gen_meanvar_df_old(mydata, dayn)
		meanvar_df <- gen_meanvar_df_new(roll_list, dayn)

		## indexes to use for mean/var analysis
		## the "-1" here is to include data from the past, and to exclude data from
		## the tradeday date.
		## 
		##            /--- trade day
		## 1 2 3 4 5 6
		## |-------|
		##         \--- indexes to use in analises
		meanvar_indexes <- ((dayn - nday_roll_window + 1):dayn) - 1
		
		######### SELECTION ALGORITHM ######### 
		######### to see what this is doing set "animate_selection_algorithm" to TRUE
		
		###### mean / variance analysis
		median_mean <- median(meanvar_df$mean)
		median_var  <- median(meanvar_df$var)
	
		## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
		}
		
		## choose three stocks.
		## if three stocks are not in the lower right quadrant
		## divide median_var by 2, until the subset dataframe contains three or more stocks
		## then sort the subset dataframe and choose the highest mean
		subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                        meanvar_df$var <= median_var, ]

		## continue sub-dividing mean/var space until we have a list of n_or_fewer stocks
		if(subdivide_until_less_than_n_stocks == T) {
		  #n_or_fewer_stocks_in_quadrant <- 20
		  while (length(subset_meanvar_df$stock_n) > n_or_fewer_stocks_in_quadrant) {
		  	## debugode
		  	n_stocks_in_subset <- length(subset_meanvar_df$stock_n)
		  	#cat(paste("n_stocks_in_subset: ", n_stocks_in_subset, "\n"))
		  	#readline("press the any key to continue...")
		  	
		  	median_mean <- median(subset_meanvar_df$mean)
		  	median_var  <- median(subset_meanvar_df$var)

            subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var <= median_var, ]
		  	

		  	## for animated plots of selection algorithm
	          if (animate_selection_algorithm == T) {
		  		viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
	          }
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
				
		while (length(subset_meanvar_df$stock_n) < hold_n_stocks) {
			#cat(paste("debug: subset_meanvar_df$stock_n < ", hold_n_stocks, "\n", sep=""))
			median_mean <- median_mean - 0.1
			subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var <= median_var, ]

            ## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
				viz_selection_algorithm(meanvar_df$var, meanvar_df$mean, median_var, median_mean)
	        }

		}
		
		#############################
		## build correlation matrix #
		#############################

		## determine which column number holds the stock's Close values
		close_column <- which(colnames(rolled_stock_data_obj[[2]][[1]]) == "Close")

		stock_price_df <- as.data.frame(sapply(rolled_stock_data_obj[[2]][subset_meanvar_df$stock_n],
                                               "[",
                                               meanvar_indexes, close_column))

		

		colnames(stock_price_df) <- subset_meanvar_df$symbol
		cor_stock_price_df <- cor(stock_price_df)

		
		## pick three stock symbols whose stock1-stock2, stock1-stock3, stock2-stock3 correlations
		## combine to the shortest distances from the origin in 3-space
		#pick_list <- pick_three_shortest_distance_to_origin(abs(cor_stock_price_df))
        #pick_list <- pick_n_shortest_distance_to_origin(abs(cor_stock_price_df), hold_n_stocks)
        pick_list <- pick_n_shortest_distance_to_origin(cor_stock_price_df, hold_n_stocks)

		picks <- pick_list$chosen_set
		#pick_cors <- pick_list$chosen_cors
		pick_distance <- pick_list$chosen_distance
	
		pick_returns <- append(pick_returns, pick_list)
		
		#correlation_tripplets <- append(correlation_tripplets, pick_cors)
		#temp_tripplets <- rbind(temp_tripplets, c(pick_cors, pick_distance))
		distances <- append(distances, pick_distance)
			
		buy_stocks <- data.frame()
		for (n in 1:length(picks)) {
          stock_n <- which(stock_symbols == picks[n])
          stock_df <- data.frame(Date    = rolled_stock_data_obj[[2]][[stock_n]][dayn, "Date"],
                                 stock_n = stock_n,
                                 symbol  = picks[n],
                                 value   = rolled_stock_data_obj[[2]][[stock_n]][dayn, "Close"],
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
			#xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_date <- as.Date(roll_list[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_close  <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_close)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "Close")
            xaction_log <- rbind(xaction_log, xaction)
        }
	} else if (any(trade_day_index_seq == dayn)) {
		## record transaction 
        for (x in 1:(length(held_stocks$stock_n))) {
			xaction_date <- as.Date(roll_list[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "s"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			xaction_stock_close  <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(date    = xaction_date,
                                  xaction = xaction_type,
                                  stock_n = xaction_stock_number,
                                  symbol  = xaction_stock_symbol,
                                  Close   = xaction_stock_close)
            
			#colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "Close", "Close")
            xaction_log <- rbind(xaction_log, xaction)
		}
 
 		#######################
		## portfolio returns ##
		#######################
		portfolio_first_day     <- (dayn - trade_every_ndays) + 1
		portfolio_owned_indexes <- portfolio_first_day:dayn
		portfolio_symbols       <- as.character(held_stocks$symbol)

		## clear this variable so error will occur if its not set
		#rm(portfolio_weights)

		## equal weighted portfolio ##
		if (portfolio_weight_strategy == "equal") {
			symbol_weight <- 1 / (length(portfolio_symbols) - 1)
			#symbol_weight <- 0.5 / (length(portfolio_symbols) - 1)
			portfolio_weights <- c(rep(symbol_weight, length(portfolio_symbols)))
			#portfolio_weights <- c(portfolio_weights, 0.5)
			
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
                                                 

		## calculate and record risk of portfolio over the past trade_every_n_days days
		## dayn = 3000
		## trade_every_n_days = 5
		## risk_day_1 <- dayn - trade_every_ndays  + 1 = 2995 + 1 = 2996
		## risk_window_seq <- risk_day_1:dayn
		risk_day_1 <- (dayn - trade_every_ndays + 1)
		risk_window_indexes <- risk_day_1:dayn
		#portfolio_risk <- three_stock_portfolio_variance(rolled_stock_data_obj,
        #                                             picks,
        #                                             c(1/3, 1/3, 1/3),
        #                                             risk_window_indexes)
        portfolio_risk <- var(portfolio_return$return)
		risk_log <- append(risk_log, portfolio_risk)
		risk_log_xaction_list[[dayn]] <- list(tail(xaction_log,3), portfolio_risk)

		#index_fund_risk_log <- append(index_fund_risk_log,
        #                              get_index_fund_risk(index_fund_df, risk_window_indexes))

		#temp_beta <- get_portfolio_beta(index_fund_df,
        #                                rolled_stock_data_obj,
        #                                picks,
        #                                risk_window_indexes)
		#portfolio_beta_log <- append(portfolio_beta_log,
        #                             get_portfolio_beta(index_fund_df,
        #                                                rolled_stock_data_obj,
        #                                                picks,
        #                                                risk_window_indexes))

        ## record in transaction log bought stocks
		for (x in 1:(length(buy_stocks$stock_n))) {
			xaction_date <- as.Date(roll_list[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- buy_stocks[x, "stock_n"]
			xaction_stock_symbol <- buy_stocks[x, "symbol"]
			xaction_stock_close  <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_close)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "Close")
            xaction_log <- rbind(xaction_log, xaction)
        }

		## if today is a trading day, then calculate the percent change in value of portfolio
		held_value <- sum(held_stocks$value)
		sold_value <- 0
		for (stock in 1:length(held_stocks$stock_n)) {
			stock_value <- roll_list[[held_stocks[stock, "stock_n"]]][dayn, "Close"] 
			sold_value  <- sold_value + stock_value
		}
		
		percent_delta <- (sold_value - held_value) / held_value

		## a list of percent changes of portfolio
		portfolio_percent_deltas_tradeday <- append(portfolio_percent_deltas_tradeday, percent_delta)

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

		held_stocks <- buy_stocks	
	}

	## keep track of portflio value every day
	todays_portfolio_value <- 0
	for (stock in held_stocks$stock_n) {
		#todays_portfolio_value <- todays_portfolio_value + mydata[[stock]]$Close[dayn]
		#todays_portfolio_value <- todays_portfolio_value + mydata[[stock]][dayn, "Close"]
		todays_portfolio_value <- todays_portfolio_value + roll_list[[stock]][dayn, "Close"]
	}

	## daily portfolio value percent deltas
	portfolio_percent_delta <- (todays_portfolio_value / sum(held_stocks$value) - 1)
	portfolio_percent_deltas_df <- rbind(portfolio_percent_deltas_df,
                                         data.frame(todays_date, portfolio_percent_delta))
	
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

#index_fund_value_on_trading_day <- append(index_fund_values, index_fund_df$Open[trade_day_index_seq])

index_fund_percents <- (index_fund_df$Close[trade_day_index_seq] /
                        index_fund_df$Close[start_index]) - 1

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
		val1 <- index_fund_df$Close[trade_day_index_seq[trade_day - 1]]
		val2 <- index_fund_df$Close[trade_day_index_seq[trade_day]]
	
		## percent changes from first index to last index
		#val_prcnts <- (index_fund_df$Close[indexes] / val1) - 1
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
	
	xrange <- c(0, length(trade_day_index_seq) + 1)

	#yrange_min <- min((portfolio_coefficient_tradeday_list - 1), index_fund_percents)
	#yrange_max <- max((portfolio_coefficient_tradeday_list - 1), index_fund_percents)
	#yrange <-  c(yrange_min, yrange_max)
	yrange <- range(portfolio_coefficient_tradeday_list - 1,
                    index_fund_percents, 
                    portfolio_return_df$gross_return - 1)
                    
	
	#plot(risk_log, type="l", xlim=xrange, ylim=yrange, col=rgb(0.5, 0.5, 0.5, alpha=0.5),
    #     axes=F, xlab="", ylab="")
	#par(new=T)
	plot(portfolio_coefficient_tradeday_list - 1,
         col="darkgreen", xlim=xrange, ylim=yrange, type="l", axes=F, xlab="", ylab="")
	par(new=T)
	plot(index_fund_percents, col="darkblue", xlim=xrange, ylim=yrange, type="l",
         main = paste("data path: ", datapath, ", index fund: ", index_fund_name, "\n",
                      nday_roll_window, " day mean/var window\n",
                      "trade every ", trade_every_ndays, " days",
                      " over ", years, " years, "))

		gross_return <- 1
		portfolio_return_df <- data.frame(Date = numeric(0),
		                                   return = numeric(0))
		for (n in 1:length(portfolio_returns)) {
		  last_row     <- length(portfolio_returns[[1]]$Date)
		  net_return   <- portfolio_returns[[n]][last_row, "return"]
		  date         <- portfolio_returns[[n]][last_row, "Date"]
		  gross_return <- gross_return * (net_return + 1)
		  return_df    <- data.frame(Date = date,
		                             net_return = net_return,
		                             gross_return = gross_return)
		
		  portfolio_return_df <- rbind(portfolio_return_df,  return_df)
		}
		
		par(new=T)
		plot(portfolio_return_df$gross_return - 1, xlim=xrange, ylim=yrange, type="l", col="green")

	if(hard_copy == T) {
		dev.off()
	}
}

## managed summary data
prcnt_ret <- round((portfolio_coefficient_tradeday - 1) * 100, 2)
managed_summary_tradeday <- summary(portfolio_percent_deltas_tradeday)
managed_stddev <- sd(portfolio_percent_deltas_tradeday)
managed_var <- var(portfolio_percent_deltas_tradeday)

### index fund summary data        
index_summary <- summary(index_fund_trade_day_delta_ratios)
index_stddev <- sd(index_fund_trade_day_delta_ratios)
index_var <- var(index_fund_trade_day_delta_ratios)

results_obj <- list(nyear_simulation = years,
                    datapath = datapath,
                    nday_roll_window = nday_roll_window,
                    trade_every_ndays = trade_every_ndays,
                    portfolio_prcnt_return = prcnt_ret,
                    managed_summary = managed_summary_tradeday,
                    managed_stddev = managed_stddev,
                    managed_var = managed_var,
                    managed_risk_summary = summary(risk_log),
                    managed_risk_log = risk_log,
                    portfolio_beta_log = portfolio_beta_log,
                    index_fund_name = index_fund_name,
                    index_summary = index_summary,
                    index_stddev = index_stddev,
                    index_var = index_var,
                    index_fund_name = index_fund_name,
                    index_fund_prcnt_return = round(tail(index_fund_percents, 1) * 100, 1),
                    index_fund_risk_log = index_fund_risk_log)

## the "+1" here is to match the results in index_fund_risk_log which, when calculating
## risk uses the Close of the stock on the day it is purchase as well as on the day it was
## sold, for the variance calculation

#rolled_index_fund_df <- add_rolled_var_column(index_fund_df, trade_every_ndays + 0)
#subset_indexes <- trade_day_index_seq[2:length(trade_day_index_seq)]
#tradeday_rolled_index_fund <- rolled_index_fund_df$rollvar[subset_indexes]
#print(summary(tradeday_rolled_index_fund))

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
average_annual_fund_return    <- fund_return / years
return_percent                <- round(fund_return * 100, 2)
average_annual_return_percent <- return_percent / years
stddev_percent                <- round(average_annual_index_fund_stddev * 100, 2)
sharpe_ratio                  <- (average_annual_fund_return - one_year_risk_free_rate) /
                                  average_annual_index_fund_stddev 
sharpe_ratio                  <- round(sharpe_ratio, 2)

cat(paste("### index fund ###\n"))
cat(paste("index fund return     : ", return_percent, "%\n", sep=""))
cat(paste("average annual return : ", round(return_percent / years, 2), "%\n", sep=""))
cat(paste("annual average std dev: ", stddev_percent, "%\n", sep=""))
cat(paste("sharpe ratio          : ", sharpe_ratio, "\n\n", sep=""))


## managed weighted portfolio ##
average_annual_portfolio_variance <- var(portfolio_return_df$gross_return - 1) / years
average_annual_portfolio_stddev   <- sqrt(average_annual_portfolio_variance)

portfolio_return                <- tail(portfolio_return_df$gross_return - 1, 1)
average_annaul_portfolio_return <- portfolio_return / years
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

## value weighted portfolio ##
average_annual_portfolio_variance <- var(portfolio_coefficient_tradeday_list - 1) / years
average_annual_portfolio_stddev   <- sqrt(average_annual_portfolio_variance)

portfolio_return                <- tail(portfolio_coefficient_tradeday_list - 1, 1)
average_annual_portfolio_return <- portfolio_return / years
return_percent                  <- round(portfolio_return * 100, 2)
stddev_percent                  <- round(average_annual_portfolio_stddev * 100, 2)
sharpe_ratio                    <- (average_annaul_portfolio_return - one_year_risk_free_rate) / 
                                   average_annual_portfolio_stddev
sharpe_ratio                    <- round(sharpe_ratio, 2)

cat(paste("### value-weighted portfolio ###\n"))
cat(paste("return                : ", return_percent, "%\n", sep=""))
cat(paste("average annual return : ", round(return_percent / years, 2), "%\n", sep=""))
cat(paste("annual average std dev: ", stddev_percent, "%\n", sep=""))
cat(paste("sharpe ratio          : ", sharpe_ratio, "\n\n", sep=""))

#source("mix_bond_with_vti0x01.R")
#source("mix_bond_with_vti.R")


