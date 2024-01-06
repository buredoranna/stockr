#library(zoo)
readfile <- F

## load a file
if (readfile == T) {
	nflx <- read.csv("data/nflx.tac", header=T)
	aapl <- read.csv("data/aapl.tac", header=T)
}

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
lookatpastndays <- 3000

## number of events to use in rolling mean
rollwindow <- 15

## set the range for data subset window
left <- length(nflx$Open) - lookatpastndays + 1
right <- length(nflx$Open)

## subset the data based on "left / right" window indexes
nflx_subset <- nflx$Open[left:right]
aapl_subset <- aapl$Open[left:right]

rollmean_nflx <- rollmean(nflx_subset, rollwindow)
rollmean_aapl <- rollmean(aapl_subset, rollwindow)

rollvar_nflx  <- rollapply(nflx_subset, rollwindow, FUN=var)
rollvar_aapl  <- rollapply(aapl_subset, rollwindow, FUN=var)

mean_var_nflx <- as.data.frame(cbind(as.matrix(rollmean_nflx, ncol=1),
                                     as.matrix(rollvar_nflx,  ncol=1)))

mean_var_aapl <- as.data.frame(cbind(as.matrix(rollmean_aapl, ncol=1),
                                     as.matrix(rollvar_aapl,  ncol=1)))

                               
colnames(mean_var_nflx) <- c("mean", "var")
colnames(mean_var_aapl) <- c("mean", "var")

jpeg("viz/img%000005d.jpg", width=600, height=600, quality=100)
for (coln in 1:length(mean_var_nflx$mean)) {
	xmin <- floor(min(mean_var_nflx$mean, mean_var_aapl$mean))
	xmax <- ceiling(max(mean_var_nflx$mean, mean_var_aapl$mean))
	xrange <- c(xmin, xmax)

	ymin <- floor(min(mean_var_nflx$var, mean_var_aapl$var))	
	ymax <- ceiling(max(mean_var_nflx$var, mean_var_aapl$var))	
	yrange <- c(ymin, ymax)
	if (coln > 1) {
		plot(mean_var_nflx[1:(coln - 1),], ylim=yrange, xlim=xrange)
		par(new=T)
		plot(mean_var_aapl[1:(coln - 1),], ylim=yrange, xlim=xrange)
		par(new=T)
		plot(mean_var_nflx[coln, ], ylim=yrange, xlim=xrange, col="red", pch=19)
		par(new=T)
		plot(mean_var_aapl[coln, ], ylim=yrange, xlim=xrange, col="blue", pch=19)
	} else {
		plot(mean_var_nflx[coln, ], ylim=yrange, xlim=xrange, col="red", pch=19)
		plot(mean_var_aapl[coln, ], ylim=yrange, xlim=xrange, col="blue", pch=19)
	}
}
dev.off()
	

