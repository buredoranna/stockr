## read in analysis functions
source("analysis0x02.r")

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read historical portfolio data
#print("reading data...")
source("readstocks.r")
#print("done")

## this strategy holds the security for hold_for_ndays which has the greater slope over 
## the number of days in the plot window 
##
## after some thinking I realized this strategy couldn't really be applied in real life
## becuase xaction day data was used in the linear model. To bring this strategy closer to 
## a "real life" strategy, the model data will include yesterday to n-days into the past
## to make a decision to xact on "today's" data.
##
## after my "corrections" the roi value went to shit so I'm gonna re-build this strategy
## currently I pass a vector of tkr opens as tkr_a, tirk_b, and tkr_m
## the new version will take the whole tkr structure not just the opens
strat0x01 <- function(tkr_a, tkr_b, tkr_m, ndays, hold_for_ndays, names) {
	## ticker currently being held
	held_tkr <- 0
	total_delta <- 0

	## build window for linear model
	win <- length(tkr_a):(length(tkr_a) - ndays)
	
	## map tkr values as percent of tkr value at t=0
	tkr_a_prcnt <- ((tkr_a / tkr_a[1]) - 1.0) * 100
	tkr_b_prcnt <- ((tkr_b / tkr_b[1]) - 1.0) * 100
	tkr_m_prcnt <- ((tkr_m / tkr_m[1]) - 1.0) * 100

	
	## iterate from past to present
	while( min(win) > 0 ) {
		## xaction day values
		xday_val <- cbind(tkr_a[min(win) - 1], tkr_b[min(win) - 1], tkr_m[min(win) - 1])

		## build history portfolio
		port <- cbind(tkr_a[win], tkr_b[win], tkr_m[win])

		## build linear models
		lm.tkr_a_prcnt <- lm(tkr_a_prcnt[win]~c(1:length(win)))
		lm.tkr_b_prcnt <- lm(tkr_b_prcnt[win]~c(1:length(win)))
		lm.tkr_m_prcnt <- lm(tkr_m_prcnt[win]~c(1:length(win)))

		## grab slopes
		slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                    coef(lm.tkr_b_prcnt)[[2]],
                    coef(lm.tkr_m_prcnt)[[2]])
		max_slope <- max(slopes)
		max_slope_col <- which.max(slopes)
		max_slope_name <- names[max_slope_col]
	
		## xaction 
		## this is for the first buy and will only happen once
		if (held_tkr == 0) {
			held_tkr <- max_slope_col
			b_at <- port[1, held_tkr]
		}
		
		## if the tkr with the max slope changes
		## sell current tkr and buy tkr with bigger slope
		if (held_tkr != max_slope_col && day_cntr %% hold_for_ndays == 0) {
			## value of currently held tkr on xday 
			# s_at <- port[1, held_tkr]
			s_at <- xday_val[1, held_tkr]
			
			## values of tkr when bought and sold
			bs <- c(b_at, s_at)
			
			## percent delta of tkr delta / tkr_buy_value
			prcnt_delta <- diff(bs) / b_at * 100
			
			## running total of percent deltas
			total_delta <- total_delta + prcnt_delta
		
			info <- paste(names[held_tkr], 
                          "buy:", round(b_at,2),
                          "sell:", round(s_at,2),
                          paste("delta: ", round(prcnt_delta, 2), "%", sep=""),
                          paste("total: ", round(total_delta, 2), "%", sep=""))
		
			## change the held tkr to the tkr with the bigger slope	
			held_tkr <- max_slope_col
		
			## set buy value of tkr
			b_at <- xday_val[1, held_tkr]

			print(info)
		}

		win <- win - 1
		day_cntr <- day_cntr + 1
	}

	## (present_value - past_value) / past_value
	present_val <- tkr_a[length(tkr_a)]
	past_val    <- tkr_a[1]
	tkr_a_prcnt_delta <- (present_val - past_val) / past_val * 100	
	info <- paste(names[1], " ", past_val, " ",
                  present_val, " ",
                  round(tkr_a_prcnt_delta, 2), "%", sep="")
	print(info)
	
	present_val <- tkr_b[length(tkr_b)]
	past_val    <- tkr_b[1]
	tkr_b_prcnt_delta <- (present_val - past_val) / past_val * 100	
	info <- paste(names[2], " ", past_val, " ",
                  present_val, " ",
                  round(tkr_b_prcnt_delta, 2), "%", sep="")
	print(info)
	
	present_val <- tkr_m[length(tkr_m)]
	past_val    <- tkr_m[1]
	market_prcnt_delta <- (present_val - past_val) / past_val * 100	
	info <- paste(names[3], " ", past_val, " ",
                  present_val, " ",
                  round(market_prcnt_delta, 2), "%", sep="")
	print(info)
}

## this strategy buys the tkr with the greater slope
strat0x02 <- function(tkr_a, tkr_b, tkr_m, ndays_in_lm_win, trade_every_n_days, names) {
	## currently held tkr
	held_tkr <- 0

	## day counter
	day_ctr <- 0
	
	## cumulative percent deltas
	cum_delta <- 0

	## buy sell values
	buy_val  <- 0
	sell_val <- 0

	## build initial window for linear model
	## min(win) == closer to present
	## max(min) == further from present (further in past)
	## +1 for the off by one error
	lm_win <- (length(tkr_a$Open) - ndays_in_lm_win + 1):length(tkr_a$Open)
	pl_win <- length(tkr_a$Open):(length(tkr_a$Open) - ndays_in_lm_win + 1)

	## map tkr open values to percent of tkr value at t=(furthest in the past)
	tkr_a_prcnt <- (tkr_a$Open / tkr_a$Open[length(tkr_a$Open)] - 1.0) * 100
	tkr_b_prcnt <- (tkr_b$Open / tkr_b$Open[length(tkr_b$Open)] - 1.0) * 100
	tkr_m_prcnt <- (tkr_m$Open / tkr_m$Open[length(tkr_m$Open)] - 1.0) * 100

	## build portfolio of tkr values over window
	port_val_win <- rbind(tkr_a$Open[pl_win],
                          tkr_b$Open[pl_win],
                          tkr_m$Open[pl_win])
	colnames(port_val_win) <- tkr_a$Date[pl_win]
	rownames(port_val_win) <- names

	## build set of xday tkr values
	xday <- min(pl_win) - 1 # "-1" means one day further into the future
	xday_tkr_vals <- rbind(tkr_a$Open[xday],
                           tkr_b$Open[xday],
                           tkr_m$Open[xday])
	colnames(xday_tkr_vals) <- tkr_a$Date[xday]
	rownames(xday_tkr_vals) <- names
	
	## build portfolio of tkr percent values
	port_prcnt_win <- rbind(round(tkr_a_prcnt[pl_win], 2),
                            round(tkr_b_prcnt[pl_win], 2),
                            round(tkr_m_prcnt[pl_win], 2))
	colnames(port_prcnt_win) <- tkr_a$Date[pl_win]
	rownames(port_prcnt_win) <- names

	## debug prints
	#print("tkr values over window")
	#print(port_val_win)
	#cat("\n")
	#
	#print("tkr values as precentage of tkr value at t=0")
	#print(port_prcnt_win)
	#cat("\n")
	#
	#print("xday tkr values")
	#print(xday_tkr_vals)
	#cat("\n")

	## build initial linear models 
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt[pl_win]~c(seq(1:length(pl_win))))
	lm.tkr_b_prcnt <- lm(tkr_b_prcnt[pl_win]~c(seq(1:length(pl_win))))
	lm.tkr_m_prcnt <- lm(tkr_m_prcnt[pl_win]~c(seq(1:length(pl_win))))

	## grab slopes
	slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                coef(lm.tkr_b_prcnt)[[2]],
                coef(lm.tkr_m_prcnt)[[2]])
	max_slope <- max(slopes)
	max_slope_col <- which.max(slopes)
	max_slope_name <- names[max_slope_col]
	
	held_tkr <- max_slope_col
	buy_val <- xday_tkr_vals[held_tkr, 1]
	day_ctr <- day_ctr + 1

	# foo <- readline("press the any key to continue...")
	## main loop buying strategy
	while(xday > 1) {
		## move plot window and xday one day into the future
		pl_win <- pl_win - 1
		xday <- xday - 1

		## build portfolio of tkr values over window
		port_val_win <- rbind(tkr_a$Open[pl_win],
                              tkr_b$Open[pl_win],
                              tkr_m$Open[pl_win])
		colnames(port_val_win) <- tkr_a$Date[pl_win]
		rownames(port_val_win) <- names

		## build set of xday tkr values
		xday <- min(pl_win) - 1 # "-1" means one day further into the future
		xday_tkr_vals <- rbind(tkr_a$Open[xday],
                               tkr_b$Open[xday],
                               tkr_m$Open[xday])
		colnames(xday_tkr_vals) <- tkr_a$Date[xday]
		rownames(xday_tkr_vals) <- names
	
		## build portfolio of tkr percent values
		port_prcnt_win <- rbind(round(tkr_a_prcnt[pl_win], 2),
                                round(tkr_b_prcnt[pl_win], 2),
                                round(tkr_m_prcnt[pl_win], 2))
		colnames(port_prcnt_win) <- tkr_a$Date[pl_win]
		rownames(port_prcnt_win) <- names

		## build linear models
		lm.tkr_a_prcnt <- lm(tkr_a_prcnt[pl_win]~c(seq(1:length(pl_win))))
		lm.tkr_b_prcnt <- lm(tkr_b_prcnt[pl_win]~c(seq(1:length(pl_win))))
		lm.tkr_m_prcnt <- lm(tkr_m_prcnt[pl_win]~c(seq(1:length(pl_win))))
	
		## grab slopes
		slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                    coef(lm.tkr_b_prcnt)[[2]],
                    coef(lm.tkr_m_prcnt)[[2]])
		max_slope <- max(slopes)
		max_slope_col <- which.max(slopes)
		max_slope_name <- names[max_slope_col]

		## debug prints
		#print("tkr values over window")
		#print(port_val_win)
		#cat("\n")
		#
		#print("tkr values as precentage of tkr value at t=0")
		#print(port_prcnt_win)
		#cat("\n")
		#
		#print("xday tkr values")
		#print(xday_tkr_vals)
		#cat("\n")
		
		## if the tkr with the bigger slope is different from the currently held tkr
		## and it is a trading day
		## sell currently held tkr
		## and buy the tkr with the bigger slope
		if(held_tkr != max_slope_col && day_ctr %% trade_every_n_days == 0) {
			## sell currently held tkr
			sell_val <- xday_tkr_vals[held_tkr,1]

			## percent change over time
			prcnt_delta <- ((sell_val - buy_val) / buy_val) * 100
	
			## keep running total of delta percentages
			cum_delta <- cum_delta + prcnt_delta

			info <- paste(names[held_tkr],
                          ": bought at ", buy_val, " ",
                          "sold at ", sell_val, " ",
                          "delta ", round(prcnt_delta, 2), "% ",
                          "cum_delta ", round(cum_delta, 2),  "%", sep="")
			print(info)

			## buy new tkr
			held_tkr <- max_slope_col
			buy_val <- xday_tkr_vals[held_tkr, 1]
		}
		day_ctr <- day_ctr + 1

		# foo <- readline("press the any key to continue...")
	}

	## debugode
	#info <- paste(names[1], " ", round(slopes[1], 2), "\n",
    #              names[2], " ", round(slopes[2], 2), "\n",
    #              names[3], " ", round(slopes[3], 2), "\n", sep="")
	#cat(info)

	
	###########
	## plots ##
	###########
	
	## build yrange
	#ymin <- min(tkr_a_prcnt[pl_win], tkr_b_prcnt[pl_win], tkr_m_prcnt[pl_win])	
	#ymax <- max(tkr_a_prcnt[pl_win], tkr_b_prcnt[pl_win], tkr_m_prcnt[pl_win])	
	#yrange <- c(ymin, ymax)
	
	## plots
	#main_info <- paste(names[1], "(r)", names[2], "(b)", names[3], "(b)")
	#
	#plot(tkr_a_prcnt[pl_win], ylim=yrange, ylab="% of value at t=0", main=main_info, col="red")
	#abline(lm.tkr_a_prcnt, col="red")
	#par(new=T)
	#
	#plot(tkr_b_prcnt[pl_win], ylim=yrange, xlab="", ylab="", axes="F", col="blue")
	#abline(lm.tkr_b_prcnt, col="blue")
	#par(new=T)
	#
	#plot(tkr_m_prcnt[pl_win], ylim=yrange, xlab="", ylab="", axes="F", col="darkgreen")
	#abline(lm.tkr_m_prcnt, col="darkgreen")

	## print single stock and market percent deltas	
	print(paste(names[1], " ", round(tkr_a_prcnt[1], 2), "%", sep=""))
	print(paste(names[2], " ", round(tkr_b_prcnt[1], 2), "%", sep=""))
	print(paste(names[3], " ", round(tkr_m_prcnt[1], 2), "%", sep=""))
}
	
## this strategy buys the tkr with the greater slope IF the greater slope is greater by
## a certain percentage
strat0x03 <- function(tkr_a, tkr_b, tkr_m,
                      ndays_in_lm_win, trade_every_n_days,
                      slope_delta_threshold, names) {
	## currently held tkr
	held_tkr <- 0

	## day counter
	day_ctr <- 0
	
	## cumulative percent deltas
	cum_delta <- 0

	## buy sell values
	buy_val  <- 0
	sell_val <- 0

	## slope deltas list
	slope_delta_list <- c()

	## build initial window for linear model
	## min(win) == closer to present
	## max(min) == further from present (further in past)
	## +1 for the off by one error
	lm_win <- (length(tkr_a$Open) - ndays_in_lm_win + 1):length(tkr_a$Open)
	pl_win <- length(tkr_a$Open):(length(tkr_a$Open) - ndays_in_lm_win + 1)

	## map tkr open values to percent of tkr value at t=(furthest in the past)
	tkr_a_prcnt <- (tkr_a$Open / tkr_a$Open[length(tkr_a$Open)] - 1.0) * 100
	tkr_b_prcnt <- (tkr_b$Open / tkr_b$Open[length(tkr_b$Open)] - 1.0) * 100
	tkr_m_prcnt <- (tkr_m$Open / tkr_m$Open[length(tkr_m$Open)] - 1.0) * 100

	## build portfolio of tkr values over window
	port_val_win <- rbind(tkr_a$Open[pl_win],
                          tkr_b$Open[pl_win],
                          tkr_m$Open[pl_win])
	colnames(port_val_win) <- tkr_a$Date[pl_win]
	rownames(port_val_win) <- names

	## build set of xday tkr values
	xday <- min(pl_win) - 1 # "-1" means one day further into the future
	xday_tkr_vals <- rbind(tkr_a$Open[xday],
                           tkr_b$Open[xday],
                           tkr_m$Open[xday])
	colnames(xday_tkr_vals) <- tkr_a$Date[xday]
	rownames(xday_tkr_vals) <- names
	
	## build portfolio of tkr percent values
	port_prcnt_win <- rbind(round(tkr_a_prcnt[pl_win], 2),
                            round(tkr_b_prcnt[pl_win], 2),
                            round(tkr_m_prcnt[pl_win], 2))
	colnames(port_prcnt_win) <- tkr_a$Date[pl_win]
	rownames(port_prcnt_win) <- names

	## debug prints
	#print("tkr values over window")
	#print(port_val_win)
	#cat("\n")
	#
	#print("tkr values as precentage of tkr value at t=0")
	#print(port_prcnt_win)
	#cat("\n")
	#
	#print("xday tkr values")
	#print(xday_tkr_vals)
	#cat("\n")

	## build initial linear models 
	lm.tkr_a_prcnt <- lm(tkr_a_prcnt[pl_win]~c(seq(1:length(pl_win))))
	lm.tkr_b_prcnt <- lm(tkr_b_prcnt[pl_win]~c(seq(1:length(pl_win))))
	lm.tkr_m_prcnt <- lm(tkr_m_prcnt[pl_win]~c(seq(1:length(pl_win))))

	## grab slopes
	slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                coef(lm.tkr_b_prcnt)[[2]],
                coef(lm.tkr_m_prcnt)[[2]])
	max_slope <- max(slopes)
	max_slope_col <- which.max(slopes)
	max_slope_name <- names[max_slope_col]
	
	held_tkr <- max_slope_col
	buy_val <- xday_tkr_vals[held_tkr, 1]
	day_ctr <- day_ctr + 1

	# foo <- readline("press the any key to continue...")
	## main loop buying strategy
	while(xday > 1) {
		## move plot window and xday one day into the future
		pl_win <- pl_win - 1
		xday <- xday - 1

		## build portfolio of tkr values over window
		port_val_win <- rbind(tkr_a$Open[pl_win],
                              tkr_b$Open[pl_win],
                              tkr_m$Open[pl_win])
		colnames(port_val_win) <- tkr_a$Date[pl_win]
		rownames(port_val_win) <- names

		## build set of xday tkr values
		xday <- min(pl_win) - 1 # "-1" means one day further into the future
		xday_tkr_vals <- rbind(tkr_a$Open[xday],
                               tkr_b$Open[xday],
                               tkr_m$Open[xday])
		colnames(xday_tkr_vals) <- tkr_a$Date[xday]
		rownames(xday_tkr_vals) <- names
	
		## build portfolio of tkr percent values
		port_prcnt_win <- rbind(round(tkr_a_prcnt[pl_win], 2),
                                round(tkr_b_prcnt[pl_win], 2),
                                round(tkr_m_prcnt[pl_win], 2))
		colnames(port_prcnt_win) <- tkr_a$Date[pl_win]
		rownames(port_prcnt_win) <- names

		## build linear models
		lm.tkr_a_prcnt <- lm(tkr_a_prcnt[pl_win]~c(seq(1:length(pl_win))))
		lm.tkr_b_prcnt <- lm(tkr_b_prcnt[pl_win]~c(seq(1:length(pl_win))))
		lm.tkr_m_prcnt <- lm(tkr_m_prcnt[pl_win]~c(seq(1:length(pl_win))))
	
		## grab slopes
		slopes <- c(coef(lm.tkr_a_prcnt)[[2]],
                    coef(lm.tkr_b_prcnt)[[2]],
                    coef(lm.tkr_m_prcnt)[[2]])
		max_slope <- max(slopes)
		max_slope_col <- which.max(slopes)
		max_slope_name <- names[max_slope_col]
		
		max_slope_delta_prcnt <- ((max_slope - slopes[held_tkr]) / slopes[held_tkr]) * 100
		if (max_slope_delta_prcnt > 0) {
			slope_delta_list <- append(slope_delta_list, max_slope_delta_prcnt)
		}
		# print(paste("max_slope_delta_prcnt:", round(max_slope_delta_prcnt), 2))
		# foo <- readline("press the any key to continue")

		## debug prints
		#print("tkr values over window")
		#print(port_val_win)
		#cat("\n")
		#
		#print("tkr values as precentage of tkr value at t=0")
		#print(port_prcnt_win)
		#cat("\n")
		#
		#print("xday tkr values")
		#print(xday_tkr_vals)
		#cat("\n")
		
		## if the tkr with the bigger slope is different from the currently held tkr
		## and it is a trading day
		## sell currently held tkr
		## and buy the tkr with the bigger slope
		if(held_tkr != max_slope_col &&
           max_slope_delta_prcnt > slope_delta_threshold &&
           day_ctr %% trade_every_n_days == 0) {
			## sell currently held tkr
			sell_val <- xday_tkr_vals[held_tkr,1]

			## percent change over time
			prcnt_delta <- ((sell_val - buy_val) / buy_val) * 100
	
			## keep running total of delta percentages
			cum_delta <- cum_delta + prcnt_delta

			info <- paste(names[held_tkr],
                          ": bought at ", buy_val, " ",
                          "sold at ", sell_val, " ",
                          "delta ", round(prcnt_delta, 2), "% ",
                          "cum_delta ", round(cum_delta, 2),  "%", sep="")
			# print(info)

			## buy new tkr
			held_tkr <- max_slope_col
			buy_val <- xday_tkr_vals[held_tkr, 1]
		}
		day_ctr <- day_ctr + 1

		# foo <- readline("press the any key to continue...")
	}

	## debugode
	#info <- paste(names[1], " ", round(slopes[1], 2), "\n",
    #              names[2], " ", round(slopes[2], 2), "\n",
    #              names[3], " ", round(slopes[3], 2), "\n", sep="")
	#cat(info)

	
	###########
	## plots ##
	###########
	
	## build yrange
	#ymin <- min(tkr_a_prcnt[pl_win], tkr_b_prcnt[pl_win], tkr_m_prcnt[pl_win])	
	#ymax <- max(tkr_a_prcnt[pl_win], tkr_b_prcnt[pl_win], tkr_m_prcnt[pl_win])	
	#yrange <- c(ymin, ymax)
	
	## plots
	#main_info <- paste(names[1], "(r)", names[2], "(b)", names[3], "(b)")
	#
	#plot(tkr_a_prcnt[pl_win], ylim=yrange, ylab="% of value at t=0", main=main_info, col="red")
	#abline(lm.tkr_a_prcnt, col="red")
	#par(new=T)
	#
	#plot(tkr_b_prcnt[pl_win], ylim=yrange, xlab="", ylab="", axes="F", col="blue")
	#abline(lm.tkr_b_prcnt, col="blue")
	#par(new=T)
	#
	#plot(tkr_m_prcnt[pl_win], ylim=yrange, xlab="", ylab="", axes="F", col="darkgreen")
	#abline(lm.tkr_m_prcnt, col="darkgreen")

	## print single stock and market percent deltas	
	print(paste(names[1], " ", round(tkr_a_prcnt[1], 2), "%", sep=""))
	print(paste(names[2], " ", round(tkr_b_prcnt[1], 2), "%", sep=""))
	print(paste(names[3], " ", round(tkr_m_prcnt[1], 2), "%", sep=""))
	print(paste("model roi:", paste(round(cum_delta, 2), "%", sep=""),
                "model window:", ndays_in_lm_win, "days,",
                "trade every", trade_every_n_days,"days,", 
                "slope threshold:", slope_delta_threshold, "percent"))

	return_data <- c(tkr_a_prcnt[1], tkr_b_prcnt[1], tkr_m_prcnt[1], cum_delta)
	return(return_data)

}

## this strategy takes a data frame of the form
##   <date>   <tkr_a open> ... <tkr_z open>
##   present  ...          ... ...
##   ...      ...          ... ...
##   past     ...          ... ...
##
## and buys the tkr with the highest expected return (slope of lm regression) AND
## the smallest std dev (as a proxy for risk) of residuals of the lm fit.
strat0x04 <- function(tkrs, days_in_lm_win, trade_every_n_days) {
	## debugode
	#print("entering strat0x04...")
	#print(head(tkrs))
	#print(paste("number of days in tkr data.frame ==", nrow(tkrs)))
	#print(paste("days_in_lm_win ==", days_in_lm_win))
	#print(paste("trade_every_n_days ==", trade_every_n_days))
	## number of tkrs in tkrs data frame
	#ntkrs <- ncol(tkrs) - 1 # "-1" for Date column

	## tkr symbols
	names <- colnames(tkrs)[2:ncol(tkrs)]

	## currently held tkr
	held_tkr <- 0

	## day counter
	# day_ctr <- 0 

	## cumulative percent deltas
	cum_delta_prcnt <- 0
	xdelta_prcnt_list <- c()
	market_delta_prcnt_list <- c(0)

	## buy / sell values
	#buy_val  <- 0
	xbuy <- data.frame()
	#sell_val <- 0
	xsell <- data.frame()

	## build lm window and
    ## xday day ate (one day further into the future than is in the lm window)
	## min(win) == closer to present
	## max(win) == further in past
	## +1 for off by one error
	pl_win <- length(tkrs$Date):(length(tkrs$Date) - days_in_lm_win + 1)
	xday <- min(pl_win) - 1
	firstxday <- xday

	## debugode	
	#print(tkrs[rev(win),])
	#return(tkrs[pl_win,1:5])

	## map tkr values to percent of tkr value at t=(furthest in the past)
	#tkrs_prcnt <- tkrs
	#for (t in 1:length(tkrs$Date)) {
	#	tkrs_prcnt[t,2:ncol(tkrs)] <- tkrs[t,2:ncol(tkrs)] / tkrs[nrow(tkrs), 2:ncol(tkrs)] - 1
	#}
	
	## debugode
	#return(tkrs_prcnt[pl_win, ])
	
	## build linear models
	lm_info <- data.frame()
	for (t in 2:ncol(tkrs)) {
		## tkr column number
		#tkr_col <- t + 1
	
		## build lm over window for given tkr	
		lm.tkr <- lm(tkrs[pl_win, t]~c(seq(1:length(pl_win))))

		## map residuals to percentage of first tkr value in lm window
		## DO NOT DO THIS (I've left this here as a reminder of what NOT to do)
		#lm_resids_prcnt <- residuals(lm.tkr) / tkrs[min(pl_win), t]
	
		## debugode	
		#print("tkr values")
		#print(tkrs[pl_win, t])
		#print("lm residuals")
		#print(residuals(lm.tkr))
		#print("lm resids as percent of first value in lm window")
		#print(lm_resids_prcnt)
		#print("residuals std dev")
		#print(sd(residuals(lm.tkr)))
		#print("... std dev as percentage of first value in lm window")
		#print(paste(sd(residuals(lm.tkr)),"/",tkrs[xday,t],"=",
        #            sd(residuals(lm.tkr)) / tkrs[xday,t]))
		#print("percent residuals std dev")
		#print(sd(lm_resids_prcnt))
		#readline("press the any key to continue...")
		#
		#plot(tkrs[pl_win, t], main=names[t])
		#abline(lm.tkr)
		#readline("press the any key to continue...")
		
		## build data frame of lm info 
		lm_info <- rbind(lm_info,
                         data.frame(tkrs$Date[max(pl_win)], # first day in model window
                                    tkrs$Date[min(pl_win)], # last day in model window
                                    tkrs$Date[xday],        # date of xaction
                                    tkrs[xday, t],          # tkr value on xaction day
                                    colnames(tkrs[t]),      # tkr name
                                    t,                      # tkr col number
                                    (coef(lm.tkr)[[2]] / tkrs[max(pl_win), t]) * 100,      # slope
                                    (sd(residuals(lm.tkr) / tkrs[max(pl_win), t]) * 100))) # sd % resids
		
		## debugode
		#print(tkrs_prcnt[pl_win, tkr_col])
		#plot(tkrs_prcnt[pl_win, tkr_col], ylim=c(-0.2, 0.2), main=names[t])
		#abline(lm.tkr)
		#print(tdf)
		#readline("press the any key to continue...")
	}
	
	## set the column names of the lm info data frame
	colnames(lm_info) <- c("lm_sdate",
                           "lm_edate",
                           "xdate",
                           "xval",
                           "tkr",
                           "tkr_col",
                           "prcnt_lm_slope",
                           "prcnt_sd_res")
	tmp_lm_info <- lm_info
	## debugode
	# print(head(lm_info))
	# print("press the any key to continue...")
	
	## exclude the lin models with negative slopes
	#tmp_lm_info <- lm_info[lm_info$prcnt_lm_slope > 0, ]

	## exlude the lin models with sd of resid-precents above sd.thresh
	#sd.thresh <- mean(tmp_lm_info$prcnt_sd_res) - sd(tmp_lm_info$prcnt_sd_res)
	#tmp_lm_info <- tmp_lm_info[tmp_lm_info$prcnt_sd_res < sd.thresh, ]
	
	## order the lm_info data frame by sd of resid-percents
	tmp_lm_info <- tmp_lm_info[order(tmp_lm_info$prcnt_sd_res), ]	
	#print(head(tmp_lm_info))
	#readline("press the any key to continue...")

	## order the lm_info data frame by slope
	#tmp_lm_info <- tmp_lm_info[order(tmp_lm_info$prcnt_lm_slope), ]
	
	## debugode
	#print(head(tmp_lm_info))
	#readline("press the any key to continue...")

	## exclude the lin models with sd of resids above 0.1
	#tkrs_of_interest <- tkrs_of_interest[tkrs_of_interest
		
	#print(ord.lm_info_pos_slopes[ord.lm_info_pos_slopes$sd_lm_resids < 0.2, ])
	#readline("press the any key to continue...")

	# buy_val <- tmp_lm_info$xval
	xbuy <- data.frame(tmp_lm_info$xdate[1],
                       tmp_lm_info$tkr[1],
                       tmp_lm_info$tkr_col[1],
                       tmp_lm_info$xval[1])
	colnames(xbuy) <- c("xdate",
                        "tkr",
                        "tkr_col",
                        "xval")

	xinfo <- paste(xbuy$xdate, "bought", xbuy$tkr, "at", xbuy$xval)
	
	## debugode
	#print(xinfo)
	#readline("press the any key to continue...")

	## debugode
	#print("tkrs ordered lm_res_sd and pos slopes...")
	#print(head(ord.lm_info_pos_slopes))
	#print("xbuy...")
	#print(xbuy)
	
	## debugode
	#buy_tkr <- ord.lm_info[ord.lm_info$lm_slope > 0, ][1,]
	#tkr_num <- which(names == buy_tkr$tkr_name)
	#print(names[tkr_num])
	#return(buy_tkr)	
	#return(head(ord.lm_info_pos_slopes))

	########################################################
	## main loop
	########################################################
	while (xday > 1) {
		## move plot window and xday one day into the future
		#pl_win <- pl_win - 1
		#xday <- xday - 1
		pl_win <- pl_win - trade_every_n_days
		xday <- min(pl_win) - 1
		if (min(pl_win) < 2) {
			pl_win <- (days_in_lm_win+1):2
			xday <- min(pl_win) - 1
		}
			
		#print("------------")
		#print(paste("pl_win", min(pl_win), max(pl_win),
        #            "xday", xday))
		#print("------------")

		## build data frame from lm info
		lm_info <- data.frame()
		for (t in 2:ncol(tkrs)) {
			## build lm over window for given tkr
			lm.tkr <- lm(tkrs[pl_win, t]~c(seq(1:length(pl_win))))

			## build data frame from lm info
        	lm_info <- rbind(lm_info,
                         data.frame(tkrs$Date[max(pl_win)], # first day in model window
                                    tkrs$Date[min(pl_win)], # last day in model window
                                    tkrs$Date[xday],        # date of xaction
                                    tkrs[xday, t],          # tkr value on xaction day
                                    colnames(tkrs[t]),      # tkr name
                                    t,                      # tkr col number
                                    (coef(lm.tkr)[[2]] / tkrs[max(pl_win), t]) * 100,      # slope
                                    (sd(residuals(lm.tkr) / tkrs[max(pl_win), t]) * 100))) # sd % resids
		}	
    	## set the column names of the lm info data frame
	    colnames(lm_info) <- c("lm_sdate",
                               "lm_edate",
                               "xdate",
                               "xval",
                               "tkr",
                               "tkr_col",
                               "prcnt_lm_slope",
                               "prcnt_sd_res")
		## dbugode
		#print(head(lm_info))
		#readline("press the any key to continue...")

		########################################################
		## main strategy
        ## buy the stock with a positive slope AND
		## the best lm fit
		########################################################

		## exclude the lin models with negative slopes
		#tmp_lm_info <- lm_info[lm_info$prcnt_lm_slope > 0, ]
	
		## exlude the lin models with sd of resid-precents above sd.thresh
		#sd.thresh <- mean(tmp_lm_info$prcnt_sd_res) - sd(tmp_lm_info$prcnt_sd_res)
		#tmp_lm_info <- tmp_lm_info[tmp_lm_info$prcnt_sd_res < sd.thresh, ]
		
		## order the lm_info data frame by sd of resid-percents
		#tmp_lm_info <- tmp_lm_info[order(tmp_lm_info$prcnt_sd_res), ]
		
		tmp_lm_info <- lm_info[order(lm_info$prcnt_sd_res), ]
	
		#head(print(tmp_lm_info))
		#readline("press the any key to continue...")
	
		## order the lm_info data frame by slope
		#tmp_lm_info <- tmp_lm_info[order(tmp_lm_info$prcnt_lm_slope), ]
		
		## debugode
		#print(head(tmp_lm_info))
		#readline("press the any key to continue...")
	
		## if the preferred tkr differs from from the currently held tkr AND
		## it is a trading day THEN
		## sell currently held tkr AND
		## buy the new tkr
		#print(tmp_lm_info)
		#print(tmp_lm_info$tkr_col)
		#readline("press the any key to continue...")
		#if (xday > 1 &&
		if (nrow(tmp_lm_info) > 0 &&
            tmp_lm_info$tkr_col != xbuy$tkr_col) {
            # xday %% trade_every_n_days == 0) {

			xsell <- data.frame(tkrs$Date[xday],
                                xbuy$tkr,
                                xbuy$tkr_col,
                                tkrs[xday, xbuy$tkr_col])
			colnames(xsell) <- c("xdate",
                                 "tkr",
                                 "tkr_col",
                                 "xval")
			
			xdelta_prcnt <- ((xsell$xval - xbuy$xval) / xbuy$xval) * 100
			xdelta_prcnt_list <- append(xdelta_prcnt_list, xdelta_prcnt)
			cum_delta_prcnt <- cum_delta_prcnt + xdelta_prcnt

			xinfo <- paste(xbuy$xdate, "bought", xbuy$tkr, "at", xbuy$xval)
			print(xinfo)
			xinfo <- paste(xsell$xdate, "sold", xsell$tkr, "at", xsell$xval,
                           "xdelta", paste(round(xdelta_prcnt, 2), "%", sep=""),
                           "cum delta prcnt", paste(round(cum_delta_prcnt, 2), "%", sep=""))
			print(xinfo)
			
			xbuy <- data.frame(tmp_lm_info$xdate[1],
                               tmp_lm_info$tkr[1],
                               tmp_lm_info$tkr_col[1],
                               tmp_lm_info$xval[1])
			colnames(xbuy) <- c("xdate",
                                "tkr",
                                "tkr_col",
                                "xval")
			
			print(paste("buying:", xbuy$tkr))
		}
	}	

	market_ret <- (((tkrs$gspc[1] -
                     tkrs$gspc[nrow(tkrs) - days_in_lm_win]) /
                     tkrs$gspc[nrow(tkrs) - days_in_lm_win]) * 100)
	
	print(paste("days in lm model:", days_in_lm_win))
	print(paste("trade very n days:", trade_every_n_days))	
	
	print(paste("market return since", tkrs$Date[firstxday],
                paste("(",length(tkrs$Date[1:firstxday]), " days)", sep=""),
                paste(round(market_ret, 2), "%", sep="")))
	print(paste("model return: ", round(cum_delta_prcnt, 2), "%", sep=""))
	
	print(paste("market return:",
                tkrs$Date[1], tkrs$gspc[1], "-", tkrs$gspc[length(tkrs$gspc)], "/",
                tkrs$gspc[length(tkrs$gspc)], "* 100 ==", round(market_ret, 2)))
                
	ret_data <- data.frame(tkrs$Date[firstxday],
                           days_in_lm_win,
                           trade_every_n_days,
                           round(market_ret, 2),
                           round(cum_delta_prcnt, 2),
                           sd(xdelta_prcnt_list),
                           (sd(tkrs$gspc[firstxday:1]) / tkrs$gspc[firstxday]) * 100)
	
	colnames(ret_data) <- c("firstxday",
                            "days_in_lm_win",
                            "trade_every_n_days",
                            "p_market_ret",
                            "p_model_ret",
                            "sd_p_model_ret",
                            "sd_p_market_ret")
	return(ret_data)
		
}

## iterate strat0x04 starting start_n_days_ago
iter_strat0x04 <- function(start_n_days_ago,
                           min_days_in_lm_win,
                           max_days_in_lm_win,
                           min_trade_every_n_days,
                           max_trade_every_n_days) {
	results <- data.frame()
	## iterate strat0x04 BUT trade_ever_n_days never excedes the number of days in the model window
	for (win_days in min_days_in_lm_win:max_days_in_lm_win) {
		for (tends in min_trade_every_n_days:max_trade_every_n_days) {
			start_days_ago <- start_n_days_ago + win_days 
			tkr_df <- build_tkr_df(start_days_ago)
			tmp_res <- strat0x04(tkr_df, win_days, tends)
			results <- rbind(results, tmp_res)
			print(results)
		}
	}
	
	return(results)
}

## hold the stock with the lowest std dev about the mean
strat0x05 <- function(tkrs_df, days_in_mean_win, trade_every_n_days) {
	## counters / cumulatives / lists
	cum_delta_prcnt <- 0
	cum_delta_prcnt_list <- c()
	xdelta_prcnt_list <- c()
	market_delta_prcnt_list <- c()

	## buy / sell xactions
	xbuy <- data.frame()
	xsell <- data.frame()

	## build model window and first xday
	pl_win <- nrow(tkrs_df):(nrow(tkrs_df) - days_in_mean_win + 1)
	xday <- min(pl_win) - 1
	firstxday <- xday

	## build means
	mean_info <- data.frame()
	for (tkr in 2:ncol(tkrs_df)) {
		mean.tkr <- mean(tkrs_df[pl_win, tkr])
		sd.tkr <- sd(tkrs_df[pl_win, tkr])
		
		## debugode
		#print(paste(colnames(tkrs_df[tkr]),
        #                     "mean", mean.tkr,
        #                     "std dev", sd.tkr))
		#readline("press the any key to continue...")

		mean_info <- rbind(mean_info,
                           data.frame(tkrs_df$Date[max(pl_win)],  # first day in mean window
                                      tkrs_df$Date[min(pl_win)],  # last day in mean window
                                      tkrs_df$Date[xday],         # date of xaction
                                      tkrs_df[xday, tkr],         # tkr value on xaction day
                                      colnames(tkrs_df[tkr]),     # tkr name
                                      tkr,                        # tkr col number
                                      mean.tkr,                   # tkr mean over window
                                      sd.tkr,                     # tkr sd over window
                                      (sd.tkr / mean.tkr) * 100)) # sd as percent of mean
		
		## debugode
		#print(tmp_mean_info)
		#readline("press the any key to continue...")
	}
	colnames(mean_info) <- c("sdate",
                             "edate",
                             "xdate",
                             "xval",
                             "tkr",
                             "tkr_col",
                             "tkr_mean",
                             "tkr_sd",
                             "tkr_sd_pom")
	
	tmp_mean_info <- mean_info
	
	## order by tkr sd percent-of-mean
	tmp_mean_info <- tmp_mean_info[order(tmp_mean_info$tkr_sd_pom), ]

	xbuy <- data.frame(tmp_mean_info$xdate[1],
                       tmp_mean_info$tkr[1],
                       tmp_mean_info$tkr_col[1],
                       tmp_mean_info$xval[1])
	colnames(xbuy) <- c("xdate",
                        "tkr",
                        "tkr_col",
                        "xval")

	xinfo <- paste(xbuy$xdate, "bought", xbuy$tkr, "at", xbuy$xval)
	
	## debugode
	#print(xinfo)

    ########################################################
    ## main loop
    ########################################################
	while (xday > 1) {
		## move trade_every_n_days into the future
		## re-compute mean / std-dev
		## and perform xaction
		pl_win <- pl_win - trade_every_n_days
		xday <- min(pl_win) - 1
		if (min(pl_win) < 2) {
			pl_win <- days_in_mean_win:1
			pl_win <- pl_win + 1
			xday <- 1
		}

		## build data frame from mean info
		mean_info <- data.frame()
		for (tkr in 2:ncol(tkrs_df)) {
        	mean.tkr <- mean(tkrs_df[pl_win, tkr])
        	sd.tkr <- sd(tkrs_df[pl_win, tkr])
		
        	mean_info <- rbind(mean_info,
                               data.frame(tkrs_df$Date[max(pl_win)],  # first day in mean window
                                          tkrs_df$Date[min(pl_win)],  # last day in mean window
                                          tkrs_df$Date[xday],         # date of xaction
                                          tkrs_df[xday, tkr],         # tkr value on xaction day
                                          colnames(tkrs_df[tkr]),     # tkr name
                                          tkr,                        # tkr col number
                                          mean.tkr,                   # tkr mean over window
                                          sd.tkr,                     # tkr sd over window
                                          (sd.tkr / mean.tkr) * 100)) # sd as percent of mean
		}
    	
		## set the column names of the mean_info data frame
		colnames(mean_info) <- c("sdate",
                                 "edate",
                                 "xdate",
                                 "xval",
                                 "tkr",
                                 "tkr_col",
                                 "tkr_mean",
                                 "tkr_sd",
                                 "tkr_sd_pom")
		tmp_mean_info <- mean_info

		## order by tkr sd percent-of-mean
		tmp_mean_info <- tmp_mean_info[order(tmp_mean_info$tkr_sd_pom), ]

		## if tkr with smallest sd-percent-of-mean is different from currently held tkr
		## buy the new tkr
		if (tmp_mean_info$tkr_col[1] != xbuy$tkr_col) {
			xsell <- data.frame(tkrs_df$Date[xday],
                                xbuy$tkr,
                                xbuy$tkr_col,
                                tkrs_df[xday, xbuy$tkr_col])
			colnames(xsell) <- c("xdate",
                                 "tkr",
                                 "tkr_col",
                                 "xval")
	
			## xaction deltas
			xdelta_prcnt <- ((xsell$xval - xbuy$xval) / xbuy$xval) * 100
			xdelta_prcnt_list <- append(xdelta_prcnt_list, xdelta_prcnt)
			cum_delta_prcnt <- cum_delta_prcnt + xdelta_prcnt
			cum_delta_prcnt_list <- append(cum_delta_prcnt_list, cum_delta_prcnt)

			xinfo <- paste(xbuy$xdate, "bought", xbuy$tkr, "at", xbuy$xval)
			print(xinfo)
			xinfo <- paste(xsell$xdate, "sold", xsell$tkr, "at", xsell$xval,
                           "xdelta", paste(round(xdelta_prcnt, 2), "%", sep=""),
                           "cum delta prcnt", paste(round(cum_delta_prcnt, 2), "%", sep=""))
			print(xinfo)

			## buy tkr
			xbuy <- data.frame(tmp_mean_info$xdate[1],
                               tmp_mean_info$tkr[1],
                               tmp_mean_info$tkr_col[1],
                               tmp_mean_info$xval[1])
			colnames(xbuy) <- c("xdate",
                                "tkr",
                                "tkr_col",
                                "xval")
			print(paste("buying:", xbuy$tkr))
		}
	}

	## market return
	market_ret <- (((tkrs_df$gspc[1] - tkrs_df$gspc[firstxday]) / tkrs_df$gspc[firstxday]) * 100)
	
	print(paste("days in mean model:", days_in_mean_win))
	print(paste("trade every n days:", trade_every_n_days))

	print(paste("market return since", tkrs_df$Date[firstxday],
                paste("(", length(tkrs_df$Date[1:firstxday]), " days)", sep=""),
                paste(round(market_ret, 2), "%", sep="")))
	
	print(paste("model return: ", round(cum_delta_prcnt, 2), "%", sep=""))
	
	print(paste("market return:",
                tkrs_df$gspc[1], "-", tkrs_df$gspc[firstxday], "/",
                tkrs_df$gspc[firstxday], "* 100 ==", round(market_ret, 2)))

	ret_data <- data.frame(tkrs_df$Date[firstxday],
                           tkrs_df$Date[1],
                           days_in_mean_win,
                           trade_every_n_days,
                           round(market_ret, 2),
                           round(cum_delta_prcnt, 2),
                           round(min(xdelta_prcnt_list), 2),
                           sd(xdelta_prcnt_list),
                           mean(xdelta_prcnt_list))
	
	colnames(ret_data) <- c("fistxday",
                            "lastxday",
                            "days_in_mean_win",
                            "trade_every_n_days",
                            "p_market_ret",
                            "p_model_ret",
                            "min_xdelta_prcnt",
                            "sd_p_model_ret",
                            "mean_xdelta_prcnt")
	return(ret_data)
	#return(cum_delta_prcnt_list)
}

iter_strat0x05 <- function(start_n_days_ago,
                           min_days_in_mean_win,
                           max_days_in_mean_win,
                           min_trade_every_n_days,
                           max_trade_every_n_days) {
	results <- data.frame()
	
	for (win_days in min_days_in_mean_win:max_days_in_mean_win) {
		for (tends in min_trade_every_n_days:max_trade_every_n_days) {
			start_days_ago <- start_n_days_ago + win_days
			tkr_df <- build_tkr_df(start_days_ago)
			tmp_res <- strat0x05(tkr_df, win_days, tends)
			results <- rbind(results, tmp_res)
			print(results)
		}
	}
	
	return(results)
}

iter_strat0x05_list <- function(start_n_days_ago_list,
                                 min_days_in_mean_win,
                                 max_days_in_mean_win,
                                 min_trade_every_n_days,
                                 max_trade_every_n_days) {
	results <- data.frame()
	
	for (win_days in min_days_in_mean_win:max_days_in_mean_win) {
		for (tends in min_trade_every_n_days:max_trade_every_n_days) {
			for (daysago in start_n_days_ago_list) {
				tkr_df <- build_tkr_df(daysago)
				tmp_res <- strat0x05(tkr_df, win_days, tends)
				results <- rbind(results, tmp_res)
				print(results)
			}
		}
	}
	return(results)
}	

## this is the "by the booK" mean variance portfolio management strategy
## wighting (omega) each security ... sum(omegas) == 1
strat0x06 <- function(tkrs_df, days_in_mean_win, trade_every_n_days) {
	## initial investment / portfolio value / omegas
	init_invest <- 10000
	port_val <- init_invest
	tkrs_in_port <- 4
	omegas <- c(0.25, 0.25, 0.25, 0.25)

	## counteres / accumulators / lists
	cum_delta_prcnt <- 0
	cum_delta_prcnt_list <- c()
	xdelta_prcnt_list <- c()
	market_delta_prcnt_list <- c()

	## buy / sell xactions
	xbuy <- data.frame()
	xsell <- data.frame()
	port <- data.frame()

	## build model window and first xday
	pl_win <- nrow(tkrs_df):(nrow(tkrs_df) - days_in_mean_win + 1)
	xday <- min(pl_win) - 1
	firstxday <- xday

	## build means
	mean_info <- data.frame()
	for (tkr in 2:ncol(tkrs_df)) {
		mean.tkr <- mean(tkrs_df[pl_win, tkr])
		sd.tkr <- sd(tkrs_df[pl_win, tkr])

		mean_info <- rbind(mean_info,
                           data.frame(tkrs_df$Date[max(pl_win)],  # first day in mean window
                                      tkrs_df$Date[min(pl_win)],  # last day in mean window
                                      tkrs_df$Date[xday],         # dat of xaction
                                      tkrs_df[xday, tkr],         # tkr value on xaction day
                                      colnames(tkrs_df[tkr]),     # tkr name
                                      tkr,                        # tkr col numbers
                                      mean.tkr,                   # tkr mean over window
                                      sd.tkr,                     # tkr sd over window
                                      (sd.tkr / mean.tkr) * 100)) # sd as percent of mean
	}
	colnames(mean_info) <- c("sdate",
                             "edate",
                             "xdate",
                             "xval",
                             "tkr",
                             "tkr_col",
                             "tkr_mean",
                             "tkr_sd",
                             "tkr_sd_pom")

	tmp_mean_info <- mean_info

	## order mean_info by tkr sd as percent-of-mean
	tmp_mean_info <- tmp_mean_info[order(tmp_mean_info$tkr_sd_pom), ]

	## build portfolio
	nshares <- c()
	share_vals <- c()
	for (stock in 1:tkrs_in_port) {
		nshares <- append(nshares,
                          floor((omegas[stock] * init_invest) / tmp_mean_info$xval[stock]))
		
		share_vals <- append(share_vals,
                             nshares[stock] * tmp_mean_info$xval[stock])
	}

	xbuy <- data.frame(tmp_mean_info$xdate[1],
                       tmp_mean_info$tkr[1],
                       tmp_mean_info$tkr_col[1],
                       tmp_mean_info$xval[1],
                       nshares[1],
                       share_vals[1],
                       tmp_mean_info$tkr[2],
                       tmp_mean_info$tkr_col[2],
                       tmp_mean_info$xval[2],
                       nshares[2],
                       share_vals[2],
                       tmp_mean_info$tkr[3],
                       tmp_mean_info$tkr_col[3],
                       tmp_mean_info$xval[3],
                       nshares[3],
                       share_vals[3],
                       tmp_mean_info$tkr[4],
                       tmp_mean_info$tkr_col[4],
                       tmp_mean_info$xval[4],
                       nshares[4],
                       share_vals[4])
                 
	colnames(xbuy) <- c("xdate",
                        "tkr1",
                        "tkr1_col",
                        "tkr1_xval",
                        "tkr1_nshares",
                        "tkr1_portval",
                        "tkr2",
                        "tkr2_col",
                        "tkr2_xval",
                        "tkr2_nshares",
                        "tkr2_portval",
                        "tkr3",
                        "tkr3_col",
                        "tkr3_xval",
                        "tkr3_nshares",
                        "tkr3_portval",
                        "tkr4",
                        "tkr4_col",
                        "tkr4_xval",
                        "tkr4_nshares",
                        "tkr4_portval")

	xinfo <- paste(xbuy$xdate, "b", "\n",
                   xbuy$tkr1_nshares, xbuy$tkr1, "at", xbuy$tkr1_xval, xbuy$tkr1_pval, "\n",
                   xbuy$tkr2_nshares, xbuy$tkr2, "at", xbuy$tkr2_xval, xbuy$tkr2_pval, "\n", 
                   xbuy$tkr3_nshares, xbuy$tkr3, "at", xbuy$tkr3_xval, xbuy$tkr3_pval, "\n", 
                   xbuy$tkr4_nshares, xbuy$tkr4, "at", xbuy$tkr4_xval, xbuy$tkr4_pval, "\n",
                   "portval", sum(share_vals), "\n")
	cat(xinfo)
	readline("press the any key to continue...")

    ########################################################
    ## main loop
    ########################################################
	while (xday > 1) {
		pl_win <- pl_win - trade_every_n_days
		xday <- min(pl_win) - 1

		## final model window
		if(min(pl_win) < 2) {
			pl_win <- days_in_mean_win:1
			pl_win <- pl_win + 1
			xday <- 1
		}

		## build data frame with mean info
		mean_info <- data.frame()
		for (tkr in 2:ncol(tkrs_df)) {
			mean.tkr <- mean(tkrs_df[pl_win, tkr])
			sd.tkr <- sd(tkrs_df[pl_win, tkr])
		
			mean_info <- rbind(mean_info,
                               data.frame(tkrs_df$Date[max(pl_win)],  # first day in mean window
                                          tkrs_df$Date[min(pl_win)],  # last day in mean window
                                          tkrs_df$Date[xday],         # date of xaction
                                          tkrs_df[xday, tkr],         # tkr value on xaction day
                                          colnames(tkrs_df[tkr]),     # tkr name
                                          tkr,                        # tkr col number
                                          mean.tkr,                   # tkr mean over window
                                          sd.tkr,                     # tkr sd over window
                                          (sd.tkr / mean.tkr) * 100)) # sd as percent of mean
		}	

		## set the column names of the mean_info data frame
		colnames(mean_info) <- c("sdate",
                                 "edate",
                                 "xdate",
                                 "xval",
                                 "tkr",
                                 "tkr_col",
                                 "tkr_mean",
                                 "tkr_sd",
                                 "tkr_sd_pom")	
		tmp_mean_info <- mean_info

		## order by sd percent-of-mean
		tmp_mean_info <- tmp_mean_info[order(tmp_mean_info$tkr_sd_pom), ]

		top_tkr_cols <- c(tmp_mean_info$tkr_col[1],
                          tmp_mean_info$tkr_col[2],
                          tmp_mean_info$tkr_col[3],
                          tmp_mean_info$tkr_col[4])
		xbuy_tkr_cols <- c(xbuy$tkr1_col,
                           xbuy$tkr2_col,
                           xbuy$tkr3_col,
                           xbuy$tkr4_col)

		if (!all(top_tkr_cols == xbuy_tkr_cols)) {
			print("old port")
			print(xbuy_tkr_cols)
			print(head(tmp_mean_info))
			print(paste(tmp_mean_info$tkr[1],
                        tmp_mean_info$tkr[2],
                        tmp_mean_info$tkr[3],
                        tmp_mean_info$tkr[4]))
			print(xbuy)
			xbuy_tkr_cols <- top_tkr_cols

			## sell current portfolio
			#xsell <- data.frame(tkrs_df$Date[xday],
            #                    xbuy$tkr1,
            #                    xbuy$tkr1_col,
            #                    xbuy$tkr1_xval,
            #                    xbuy$tkr1_nshares,
            #                    xbuy$tkr1_c
                                

                                
					
			print("top_tkr_cols")
            print(top_tkr_cols)
			print("xbuy_tkr_cols")
            print(xbuy_tkr_cols)
			# readline("balancing portfolio...")
		}
	}
}

## returns data frame of:
## "sdate", "edate", "tkr", "tkr_val", "sd", "mean", "sd_pom")
sd_pom_tkr_df <- function(tkr_df) {
	sdate <- tkr_df$Date[nrow(tkr_df)]
	edate <- tkr_df$Date[1]
	ndays <- nrow(tkr_df)

	cnames <- colnames(tkr_df)
	
	# ret_df <- data.frame(sdate, edate, ndays)
	ret_df <- data.frame()
	for (tkr in 2:ncol(tkr_df)) {
		sd.tkr <- sd(tkr_df[, tkr])
		mean.tkr <- mean(tkr_df[, tkr])
		
		sd_pom.tkr_df <- data.frame(sdate,
                                    edate,
                                    names(tkr_df)[tkr],
									tkr_df[1, tkr],
                                    sd.tkr,
                                    mean.tkr,
                                    round((sd.tkr / mean.tkr) * 100, 2))
	
		ret_df <- rbind(ret_df, sd_pom.tkr_df)
	}

	colnames(ret_df) <- c("sdate", "edate", "tkr", "tkr_val", "sd", "mean", "sd_pom")
	return(ret_df)
}

iter_sd_pom <- function(tkr_df, days_in_win) {
	win <- (nrow(tkr_df) - days_in_win + 1):nrow(tkr_df) # +1 for off by one error
	xday <- min(win) - 1
	
	sd_pom.df <- data.frame()
	tripple <- data.frame()
	
	while (xday > 0) {
		xbuy <- data.frame()

		## get sd-pom over window	
		ret_df <- sd_pom_tkr_df(tkr_df[win, ])

		## build sd-pom data frame
		sd_pom.df <- rbind(sd_pom.df, ret_df$sd_pom)

		## set column names for sd_pom data frame
		colnames(sd_pom.df) <- names(tkr_df[2:ncol(tkr_df)])
	
		ret_df <- ret_df[order(ret_df$sd_pom), ]

		#readline("press the any key to continue...")

		top_tkrs <- head(ret_df[ret_df$sd_pom > 1, ], 3)
		tripple <- rbind(tripple, top_tkrs$sd_pom)
		colnames(tripple) <- c("tkr1", "tkr2", "tkr3")
		
		for (t in 1:3) {
			xbuy <- rbind(xbuy,
                          data.frame(tkr_df$Date[xday],
                                     top_tkrs$tkr[t],
                                     tkr_df[xday, names(tkr_df) == top_tkrs$tkr[t]]))
		}
		colnames(xbuy) <- c("xdate", "tkr", "tkr_val")
	
		print(top_tkrs)
		print(xbuy)
		# print(sd_pom.df)
		# readline("press the any key to continue..")
		if (nrow(top_tkrs) < 3) {
			print("--- this shouldn't happen ---")
			readline("press the any key to continue...")
		}
		
		win <- win - days_in_win
		xday <- min(win) - 1
	}
	return(sd_pom.df)
	#return(tripple)
}

plot.sd_pom_df <- function(sd_pom.df, n_day_sd, over_n) {
	yrange = c(min(sd_pom.df), max(sd_pom.df))
	mean.sd <- c()

	info <- paste(n_day_sd,"-day sd-pom over ", over_n, " days", sep="")
	
	plot(sd_pom.df[,1], type="l", ylim=yrange, axes=F, xlab="", ylab="", main=info)
	for (tkr in 2:ncol(sd_pom.df)) {
		par(new=T)
		plot(sd_pom.df[, tkr], type="l", ylim=yrange, axes=F, xlab="", ylab="")
	
	}
	
	for (r in 1:nrow(sd_pom.df)) {
		tmp <- t(sd_pom.df[r, ])
		mean.sd <- append(mean.sd, mean(tmp))
	}

	par(new=T)
	plot(sd_pom.df[ ,"gspc"], col="orange", type="l", lwd="2", ylim=yrange, axes=F, xlab="", ylab="")

	par(new=T)
	plot(mean.sd, col="green", type="l", lwd="2", ylim=yrange, axes=F, xlab="", ylab="")
	
	## plot mean of sd-pom
	axis(1)
	axis(2)
}



		
