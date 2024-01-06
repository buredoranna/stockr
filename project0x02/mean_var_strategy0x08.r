require(zoo)

## this does a slightly different method of mean / variance analysis
## previously I did "sort by mean, choose top 10, then sort by variance and choose top 3"
##
## this method segments the mean / var space (x-axis = mean, y-axis = variance)
## using the median mean and median variance, and from those four areas, chooses the stocks 
## in the lower right quadrant.
##
## after sub-dividing the mean/var space into quadrants
## a correlation analysis is done
## three stocks are chosen that have the lowest correlation among them
##
## mean_var_strategy0x08.r
## this is a refinement of 0x06... gonna try using "rollmean" and "rollapply" 

## data frame for storing rolling mean/vars
## rollmean and rollvar columns are NA padded
## <Date> <AdjClose> <rollmean> <rollvar>
#roll_df <- data.frame()


## this is all necessary for simulations to work

#datapath <- "sp500/"
#stock_symbols <- list.files(datapath)

#load("sp500_25day_roll.Rdata")
#nday_roll_window <- sp500_25day_roll[[1]]$nday_roll_window
#stock_symbols <- sp500_25day_roll[[1]]$stock_symbols
#roll_list <- sp500_25day_roll[[2]]

## number of days in roll window
index_fund_name <- "sp500"

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
years <- round(runif(1, 1, 8), 2)
years <- 8

start_ndays_ago <- round(365*years) - 0

## number of values to use in mean/var calculations
nday_meanvar_window <- nday_roll_window

## trade every n days
trade_every_ndays <- sample(5:20, 1)
trade_every_ndays <- 5

readdata <- F
rolldata <- F ## if F, rolling mean/var is calculated on the fly, otherwise it is rolled

plot_meanvars <- F
animate_selection_algorithm <- T ## provide visualization of stock selection algorithm
hard_copy <- F ## if true, output performance graph to jpeg
builddata <- T
plotdata <- T
display_histograms <- F
datapath <- "dow_and_tech/"
datapath <- "sp500_and_tech/"
datapath <- "nasdaq1000/"
datapath <- "dow/"
datapath <- "sp500/"
index_fund_trade_day_prcnt_deltas <- c()


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
	if (index_fund_name == "dow") {
		index_fund_df <- read.csv("indexfund/dia", header=T)
	} 
	if (index_fund_name == "sp500") {
		index_fund_df <- read.csv("indexfund/gspc", header=T)
	}
}

read_index_fund_fn <- function(index_fund_name) {
    if (index_fund_name == "dow") {
        index_fund_df <- read.csv("indexfund/dia", header=T)
    }
    if (index_fund_name == "sp500") {
        index_fund_df <- read.csv("indexfund/gspc", header=T)
    }
	return(index_fund_df)
}

index_fund_df <- read_index_fund_fn(index_fund_name)

## this function returns a stock data object
## data is read from the datapath passed to the function
read_data_fn <- function(dataset_name, datapath) {
	#import_list <- list()
	data_list <- list()
	stock_symbols <- list.files(datapath)
    for (f in 1:length(stock_symbols)) {
        fullpath <- paste(datapath, stock_symbols[f], sep="")
        print(fullpath)
        data_list[[f]] <- read.csv(fullpath, header=T)
    }

	meta <- list(dataset_name = dataset_name,
                 datapath = datapath,
                 stock_symbols = stock_symbols)
	
	return_list <- list(meta, data_list)
	cat(paste("[[1]] == metadata\n"))
	cat(paste("[[2]] == data\n"))
	return(return_list)
}


###################################################
## build "rolling mean/var" data frame of stocks ##
###################################################
roll_data_fn <- function(stock_data_obj, nday_roll_window) {
	## the stock data passed to this function is a list() returned from read_data_fn where
    roll_list <- list()
	
	#dataset_name  <- stock_data_obj[[2]][[which(stock_data_obj[[1]] == "name_of_dataset")]]
	dataset_name <- stock_data_obj[[1]]$dataset_name
	#stock_symbols <- stock_data_obj[[2]][[which(stock_data_obj[[1]] == "stock_symbols")]]
	stock_symbols <- stock_data_obj[[1]]$stock_symbols
	stock_data    <- stock_data_obj[[2]]

	cat("building \"rolling mean/var\" data frame... ")

	## number of NAs to pad roll vector with
	## the number of NAs to use as padding is one less than the number of days in the roll window
	## because the last day in the roll window is included in the roll
	##
	## example with "nday_roll_window = 5"
	## 
	##          Date AdjClose rollmean rollvar
	##  1 2003-01-16  +-27.67-+     NA      NA
	##  2 2003-01-17  | 27.35 |     NA      NA
	##  3 2003-01-21  | 26.62 |     NA      NA
	##  4 2003-01-22  | 25.98 |     NA      NA
	##  5 2003-01-23  +-26.12-+-26.748-0.55327--> the 1st five rows are used for the 1st rolled value
	##  6 2003-01-24    24.96   26.206 0.77278
	
	NA_padding <- rep(NA, nday_roll_window - 1)
	
	cat(paste("n symbols: ", length(stock_data), "\n"))
    for (stock_n in 1:length(stock_data)) {
        stock_rollmean <- c(NA_padding, rollmean(stock_data[[stock_n]]$AdjClose, nday_roll_window))
        stock_rollvar  <- c(NA_padding, rollapply(stock_data[[stock_n]]$AdjClose, nday_roll_window, var))
        roll_df <- data.frame(stock_data[[stock_n]]$Date,
                              stock_data[[stock_n]]$Open,
                              stock_data[[stock_n]]$High,
                              stock_data[[stock_n]]$Low,
                              stock_data[[stock_n]]$Close,
                              stock_data[[stock_n]]$Volume,
                              stock_data[[stock_n]]$AdjClose,
                              stock_rollmean,
                              stock_rollvar)
        colnames(roll_df) <- c("Date", "Open", "High", "Low", "Close",
                               "Volume","AdjClose", "rollmean", "rollvar")
        roll_list[[stock_n]] <- roll_df
    }
    cat("done\n")
	#meta_names <- c("dataset_name", "stock_symbols", "nday_roll_window")
	meta_data  <- list(dataset_name, stock_symbols, nday_roll_window)
	meta <- append(stock_data_obj[[1]], list(nday_roll_window = nday_roll_window))
	
	#return_list <- list(meta_names, meta_data, roll_list)
	return_list <- list(meta, roll_list)
	return(return_list)
}

#######################################
## algorithm-visualization functions ##
#######################################
viz_selection_algorithm <- function(x, y, hline, vline) {
    plot(x, y, xlim=c(0,200), ylim=c(0,2), pch=20)
    abline(h = hline)
    abline(v = vline)
}

viz_buy_stocks <- function(x, y) {
	#main_label <- paste(chosen_tripplet, "distance", chosen_distance)
    #plot(x, y, xlim=c(0,100), ylim=c(0,1), pch=20, col="green", main=main_label)
    plot(x, y, xlim=c(0,200), ylim=c(0,2), pch=20, col="green")
	
}

if (rolldata == T) {
	roll_list <- roll_data_fn(mydata, nday_roll_window)
}

#distances <- c()
#chosen_distance <- c()
#chosen_tripplet <- c()
pick_three_shortest_distance_to_origin <- function(cor_df) {
	coms <- combn(rownames(cor_df), 3)
	distances <- c()
	return_list <- list()

	for (tripplet_column in 1:length(coms[1,])) {
		cor1 <- cor_stock_price_df[coms[1,tripplet_column], coms[2,tripplet_column]]
		cor2 <- cor_stock_price_df[coms[1,tripplet_column], coms[3,tripplet_column]]
		cor3 <- cor_stock_price_df[coms[2,tripplet_column], coms[3,tripplet_column]]

		temp <- distance_from_origin(cor1, cor2, cor3)
		distances <- append(distances, temp)
	}
	lowest_column <- which(distances == min(distances))


	cor1 <- cor_stock_price_df[coms[1,lowest_column], coms[2,lowest_column]]
	cor2 <- cor_stock_price_df[coms[1,lowest_column], coms[3,lowest_column]]
	cor3 <- cor_stock_price_df[coms[2,lowest_column], coms[3,lowest_column]]
		
	chosen_tripplet <- coms[, lowest_column]
	chosen_cors <- c(cor1, cor2, cor3)
	chosen_distance <- min(distances)
	#cat(paste("tripplet: ", chosen_tripplet, "\n"))
	#cat(paste("distance: ", chosen_distance, "\n"))
	
	## I want to return a list which contains
	## 1) three chosen stock symbols
	## 2) three correlations between those stocks
	## 3) the distance from that point to the origin
	return_list[[1]] <- chosen_tripplet
	return_list[[2]] <- chosen_cors
	return_list[[3]] <- chosen_distance

	#return(chosen_tripplet)
	return(return_list)
}


distance_from_origin <- function(cor1, cor2, cor3) {
	return(sqrt(cor1^2 + cor2^2 + cor3^2))
}
	

stock_number_seq <- 1:length(stock_symbols)

gen_meanvar_df_new <- function(roll_list_p, dayn_p) {
	n_stock_symbols <- length(roll_list_p)
	
	adjclose_column <- which(colnames(roll_list_p[[1]]) == "AdjClose")
	rollmean_column <- which(colnames(roll_list_p[[1]]) == "rollmean")
	rollvar_column  <- which(colnames(roll_list_p[[1]]) == "rollvar")

	## dayn_p is offset by one because the nth row is included in an n-day rolled value
	## I don't want to use "today's adj close" in choosing which stock, I only want to use
	## yesterday's rolled value, that is:
	## I want the rolled value that does not include today's AdjClose in the rolled calculation.
	## if the rolled values had a 3-day roll, then on Thursday, I would used the mean of the
	## adjcloses from Mon, Tues, Wed.
	##
	## again: today is the trade day, so I can only uses adjusted closes from the past, not from
	## the day I actually trade on.
	##
	## and I use the adjclose from "today" because, after the market closes, that would be the
	## adj close of the stock
	trade_day_stock_values <- unlist(lapply(roll_list_p, "[", dayn_p,     adjclose_column)) # =M=
	trade_day_rollmeans    <- unlist(lapply(roll_list_p, "[", dayn_p - 1, rollmean_column)) # =M= 
	trade_day_rollvars     <- unlist(lapply(roll_list_p, "[", dayn_p - 1, rollvar_column))  # =M=

	date_indexes <- rep(dayn_p, n_stock_symbols)
	trade_day_dates <- rep(as.Date(roll_list[[1]][dayn_p, "Date"]), n_stock_symbols)

	meanvar_df <- data.frame(stock_number_seq,
                             stock_symbols,
                             trade_day_rollmeans,
                             trade_day_rollvars,
                             trade_day_stock_values,
                             date_indexes,
                             trade_day_dates)
	colnames(meanvar_df) <- c("stock_n", "symbol", "mean", "var", "value", "date_index", "date")
	return(meanvar_df)
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
#start_index <- length(mydata[[1]]$AdjClose) - start_ndays_ago
start_index <- length(roll_list[[1]]$AdjClose) - start_ndays_ago

## "last_index" is the index of the last day in the stock data frame
#last_index <- length(mydata[[1]]$AdjClose)
last_index <- length(roll_list[[1]]$AdjClose)

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
	                                    roll_list[[s]][trade_day_index_seq, "AdjClose"] ## 
	}
	
	tech_portfolio_percent_value <- (tech_portfolio_percent_value / tech_portfolio_percent_value[1]) - 1
}

tradeday_coefficient_list <- c(1)

## a vector of distances from origin of chosen stocks in 3-space
pick_returns <- list()
distances <- c()
correlation_tripplets <- c()

## start start_ndays_ago days ago
## implement buying strategy, and keep track of portfolio value every day so I can get a good
## gauge on trading strategy variance

## for visuals
if (plot_meanvars == T || animate_selection_algorithm == T) {
	jpeg("tempviz/img%000005d.jpg", width=600, height=600, quality=100)
}
#for (dayn in start_index:last_index) {
for (dayn in trade_day_index_seq) { 
	todays_date <- as.Date(roll_list[[1]]$Date[dayn])

	## if today is a trading day calculate mean/var for each stock on the given trading day
	## and rebalance portfolio
	if (any(trade_day_index_seq == dayn)) {
		trade_day_date <- todays_date
		#trading_days <- append(trading_days, as.Date(temp))
		
		cat(paste("trade day: ", as.Date(trade_day_date), "\n"))

		## build data frame of all stocks with mean and variances within nday_meanvar_window

		meanvar_df <- data.frame()
		#meanvar_df <- gen_meanvar_df_old(mydata, dayn)
		meanvar_df <- gen_meanvar_df_new(roll_list, dayn)

		## indexes to use for mean/var analysis
		## the "-1" here is to include data from the past, and to exclude data from
		## the tradeday date.
		## 
		##           +---- trade day
        ##           |
		## 1 2 3 4 5 6
		## |-------|
		##         +--- indexes to use in analises
		meanvar_indexes <- ((dayn - nday_meanvar_window + 1):dayn) - 1
		
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
		#while (median_var >= 1) {
		#	median_var <- median_var / 2
		#}

		## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			#plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
			#abline(v=median_mean)
			#abline(h=median_var)
			viz_selection_algorithm(meanvar_df$mean, meanvar_df$var, median_var, median_mean)
		}

		## continue sub-dividing mean/var space until we have a list of n_or_fewer stocks
		n_or_fewer_stocks_in_quadrant <- 20
		while (length(subset_meanvar_df$stock_n) > n_or_fewer_stocks_in_quadrant) {
			median_mean <- median(subset_meanvar_df$mean)
			median_var  <- median(subset_meanvar_df$var)

            subset_meanvar_df <- meanvar_df[meanvar_df$mean >= median_mean &
                                            meanvar_df$var <= median_var, ]

			## for animated plots of selection algorithm
	        if (animate_selection_algorithm == T) {
				viz_selection_algorithm(meanvar_df$mean, meanvar_df$var, median_var, median_mean)
	            #plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
	            #abline(v=median_mean)
	            #abline(h=median_var)
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
                viz_selection_algorithm(meanvar_df$mean, meanvar_df$var, median_var, median_mean)
	            #plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
	            #abline(v=median_mean)
	            #abline(h=median_var)
	        }

		}
		
		#############################
		## build correlation matrix #
		#############################

		#stock_adjclose_column <- 7	
		#stock_price_df <- as.data.frame(sapply(mydata[subset_meanvar_df$stock_n],
        #                                       "[",
        #                                       meanvar_indexes, stock_adjclose_column))
		adjclose_column <- which(colnames(roll_list[[1]]) == "AdjClose")
		stock_price_df <- as.data.frame(sapply(roll_list[subset_meanvar_df$stock_n],
                                               "[",
                                               meanvar_indexes, adjclose_column))
		

		colnames(stock_price_df) <- subset_meanvar_df$symbol
		cor_stock_price_df <- cor(stock_price_df)

		buy_stocks <- data.frame()
		#stock1 <- as.character(subset_meanvar_df[order(subset_meanvar_df$mean), ][1, "symbol"])
		#temp <- abs(cor_stock_price_df)[stock1, ]
		#temp <- temp[order(temp)]
		#stock2 <- names(temp)[1]
		#stock3 <- names(temp)[2]
		#stock1_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock1, ]
		#stock2_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock2, ]
		#stock3_df <- subset_meanvar_df[subset_meanvar_df$symbol == stock3, ]
		#buy_stocks <- rbind(buy_stocks, stock1_df, stock2_df, stock3_df)
		
		#picks <- pick_three_shortest_distance_to_origin(abs(cor_stock_price_df))[[1]]
		pick_list <- pick_three_shortest_distance_to_origin(abs(cor_stock_price_df))
		picks <- pick_list[[1]]
		pick_cors <- pick_list[[2]]
		pick_distance <- pick_list[[3]]
	
		pick_returns <- append(pick_returns, pick_list)
		
		correlation_tripplets <- append(correlation_tripplets, pick_cors)
		distances <- append(distances, pick_distance)
			
		stock1_df <- subset_meanvar_df[subset_meanvar_df$symbol == picks[1], ]
        stock2_df <- subset_meanvar_df[subset_meanvar_df$symbol == picks[2], ]
        stock3_df <- subset_meanvar_df[subset_meanvar_df$symbol == picks[3], ]

		buy_stocks <- rbind(buy_stocks, stock1_df, stock2_df, stock3_df)
	
        ## for animated plots of selection algorithm
		if (animate_selection_algorithm == T) {
			viz_selection_algorithm(meanvar_df$mean, meanvar_df$var, median_var, median_mean)
			par(new=T)
			#plot(meanvar_df$mean, meanvar_df$var, xlim=c(0, 100), ylim=c(0,1), pch=20)
			#abline(v=median_mean)
			#abline(h=median_var)
			#par(new=T)
			#plot(buy_stocks$mean, buy_stocks$var, xlim=c(0,100), ylim=c(0,1), pch=20, col="green")
			viz_buy_stocks(buy_stocks$mean, buy_stocks$var)
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
			#xaction_stock_adjclose <- mydata[[xaction_stock_number]][dayn, "AdjClose"] ## 
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"] ## 
			xaction_stock_close <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_adjclose,
                                  xaction_stock_close)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "AdjClose", "Close")
            xaction_log <- rbind(xaction_log, xaction)
        }
	} else if (any(trade_day_index_seq == dayn)) {
		## record in transaction log sold stock 
        for (x in 1:(length(held_stocks$stock_n))) {
			#xaction_date <- as.Date(mydata[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_date <- as.Date(roll_list[[held_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "s"
			xaction_stock_number <- held_stocks[x, "stock_n"]
			xaction_stock_symbol <- held_stocks[x, "symbol"]
			#xaction_stock_adjclose <- mydata[[xaction_stock_number]][dayn, "AdjClose"] ## 
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"] ## 
			xaction_stock_close <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_adjclose,
                                  xaction_stock_close)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "AdjClose", "Close")
            xaction_log <- rbind(xaction_log, xaction)
		}

        ## record in transaction log bought stocks
		for (x in 1:(length(buy_stocks$stock_n))) {
			#xaction_date <- as.Date(mydata[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_date <- as.Date(roll_list[[buy_stocks[x, "stock_n"]]]$Date[dayn])
			xaction_type <- "b"
			xaction_stock_number <- buy_stocks[x, "stock_n"]
			xaction_stock_symbol <- buy_stocks[x, "symbol"]
			#xaction_stock_adjclose <- mydata[[xaction_stock_number]][dayn, "AdjClose"]
			xaction_stock_adjclose <- roll_list[[xaction_stock_number]][dayn, "AdjClose"]
			xaction_stock_close <- roll_list[[xaction_stock_number]][dayn, "Close"]

            xaction <- data.frame(xaction_date,
                                  xaction_type,
                                  xaction_stock_number,
                                  xaction_stock_symbol,
                                  xaction_stock_adjclose,
                                  xaction_stock_close)
            
			colnames(xaction) <- c("date", "xaction", "stock_n", "symbol", "AdjClose", "Close")
            xaction_log <- rbind(xaction_log, xaction)
        }

		## if today is a trading day, then calculate the percent change in value of portfolio
		held_value <- sum(held_stocks$value)
		sold_value <- 0
		for (stock in 1:length(held_stocks$stock_n)) {
			#stock_value <- mydata[[held_stocks[stock, "stock_n"]]][dayn, "AdjClose"] ## 
			stock_value <- roll_list[[held_stocks[stock, "stock_n"]]][dayn, "AdjClose"] ## 
			
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
		#todays_portfolio_value <- todays_portfolio_value + mydata[[stock]][dayn, "AdjClose"]
		todays_portfolio_value <- todays_portfolio_value + roll_list[[stock]][dayn, "AdjClose"]
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

#index_fund_value_on_trading_day <- append(index_fund_values, index_fund_df$Open[trade_day_index_seq])

index_fund_percents <- (index_fund_df$AdjClose[trade_day_index_seq] / index_fund_df$AdjClose[start_index]) - 1

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
		val1 <- index_fund_df$AdjClose[trade_day_index_seq[trade_day - 1]]
		val2 <- index_fund_df$AdjClose[trade_day_index_seq[trade_day]]
	
		## percent changes from first index to last index
		#val_prcnts <- (index_fund_df$AdjClose[indexes] / val1) - 1
		#prcnt_delta_on_trade_day <- tail(val_prcnts,1)
		prcnt_delta_on_trade_day <- (val2 - val1) / val1
	}

	index_fund_trade_day_prcnt_deltas <- append(index_fund_trade_day_prcnt_deltas,
                                                prcnt_delta_on_trade_day)
}


if (plotdata == T) {
	if (hard_copy == T) {
		outfile <- paste("tempviz/",
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


        yrange_max <- max((portfolio_coefficient_tradeday_list - 1),
                      index_fund_percents)
        
		#yrange_min <- floor(yrange_min)
        #yrange_max <- ceiling(yrange_max)
        yrange <-  c(yrange_min, yrange_max)
	}


	if (datapath == "tech/") {
		plot(tech_portfolio_percent_value, col="darkred", xlim=xrange, ylim=yrange, type="l")
		par(new=T)
	}
	
	plot(portfolio_coefficient_tradeday_list - 1, col="darkgreen", xlim=xrange, ylim=yrange, type="l")
	par(new=T)
	plot(index_fund_percents, col="darkblue", xlim=xrange, ylim=yrange, type="l",
         main = paste("data path: ", datapath, ", index fund: ", index_fund_name, "\n",
                      nday_meanvar_window, " day mean/var window\n",
                      "trade every ", trade_every_ndays, " days",
                      " over ", years, " years, "))

	if(hard_copy == T) {
		dev.off()
	}
}

## display results
cat(paste("###### ", years, " year(s) ", datapath, " trading history ######\n", sep=""))
cat(paste("meanvar window: ", nday_meanvar_window, " ", sep=""))
cat(paste("trade every: ", trade_every_ndays, "\n", sep=""))
#cat(paste("dominance: ", selection_dominance, "\n\n", sep=""))

## managed summary data
prcnt_ret <- round((portfolio_coefficient_tradeday - 1) * 100, 2)
managed_summary_tradeday <- summary(portfolio_percent_deltas_tradeday)
managed_stddev <- sd(portfolio_percent_deltas_tradeday)
managed_var <- var(portfolio_percent_deltas_tradeday)

cat(paste("managed ", datapath, " return: ", prcnt_ret, "%\n", sep=""))
cat(paste("\n%%% PERCENTS %%%\n"))
cat(paste("managed summary tradeday: portfolio_percent_deltas_tradeday\n"))
print(round(managed_summary_tradeday * 100, 3))
cat(paste("std dev: ", round(managed_stddev * 100, 4), "\n"))
#cat(paste("var    : ", round(managed_var, 4), "\n"))


## index summary data        
index_summary <- summary(index_fund_trade_day_prcnt_deltas)
index_stddev <- sd(index_fund_trade_day_prcnt_deltas)
index_var <- var(index_fund_trade_day_prcnt_deltas)

cat(paste("\n\n", index_fund_name, " return: ",
          round(tail(index_fund_percents,1) * 100, 1), "%\n", sep=""))
cat("index summary tradeday: index_fund_trade_day_prcnt_deltas \n")
cat(paste("\n%%% PERCENTS %%%\n"))
print(round(index_summary * 100, 3))
cat(paste("std dev: ", round(index_stddev * 100, 4), "\n"))
#cat(paste("var    : ", round(index_var, 4), "\n"))



## build histotrams and density plots
if (display_histograms == T) {
	xrange_min <- min(as.numeric(managed_summary_tradeday[1]),
	                  as.numeric(index_summary[1]))
	
	xrange_max <- max(as.numeric(managed_summary_tradeday[6]),
	                  as.numeric(index_summary[6]))
	
	xrange_max <- max(abs(xrange_min), xrange_max)
	xrange_max <- round(xrange_max, 2)
	xrange_min <- -xrange_max
	
	
	managed_hist <- hist(portfolio_percent_deltas_tradeday, breaks=20, plot=F)
	index_hist   <- hist(index_fund_trade_day_prcnt_deltas, breaks=20, plot=F)
	
	managed_density <- density(portfolio_percent_deltas_tradeday)
	index_density <- density(index_fund_trade_day_prcnt_deltas)
	
	yrange_max <- max(managed_hist$counts, index_hist$counts)
	
	xrange <- c(xrange_min, xrange_max)
	yrange <- c(0, yrange_max)
	
	plot(index_hist, xlim=xrange, ylim=yrange, border="darkblue")
	par(new=T)
	plot(managed_hist, xlim=xrange, ylim=yrange, border="darkgreen")
}
	


#x11()

#plot(hist(portfolio_percent_deltas_df$portfolio_percent_delta, breaks=50))
#abline(v=mean(portfolio_percent_deltas_df$portfolio_percent_delta))




