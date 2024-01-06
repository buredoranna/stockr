## I've written these functions for working with stock data downloaded from yahoo finance
require(zoo)

read_index_fund_fn <- function(index_fund_name) {
    if (index_fund_name == "dow") {
        index_fund_df <- read.csv("indexfund/dia", header=T)
    }
    if (index_fund_name == "sp500") {
        index_fund_df <- read.csv("indexfund/gspc", header=T)
    }
 return(index_fund_df)
}

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
  # computes the rolling mean and rolling variance of stock values
  #
  # Args:
  #   stock_data_obj: a stock data object returned by read_data_fn
  #     [[1]] == metadata
  #     [[2]] == stock data              
  #
  #   nday_roll_window: the number of elements (days) to include in roll window

  ## storage for rolled data
  roll_list <- list()
 
 dataset_name <- stock_data_obj[[1]]$dataset_name
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
 #metadata  <- list(dataset_name, stock_symbols, nday_roll_window)
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
 trade_day_stock_values <- unlist(lapply(roll_list_p, "[", dayn_p,     adjclose_column))
 trade_day_rollmeans    <- unlist(lapply(roll_list_p, "[", dayn_p - 1, rollmean_column))
 trade_day_rollvars     <- unlist(lapply(roll_list_p, "[", dayn_p - 1, rollvar_column))

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

