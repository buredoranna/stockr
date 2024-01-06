readfile <- F

## load a file
if (readfile == T) {
	nflx <- read.csv("data/nflx.tac", header=T)
}

## starting from the most recent event, look n-days into the past
## set to "-1" for all days
lookatpastndays <- 3000

## number of events to use in rolling mean
rollwindow <- 15

## set the range for data subset window
if (lookatpastndays > 0) {
	left <- length(nflx$Open) - lookatpastndays + 1
	right <- length(nflx$Open)

	#yrange <- c(floor(min(nflx$Open)), ceiling(max(nflx$Open)))
	yrange <- c(floor(min(nflx$Open[left:right])), ceiling(max(nflx$Open[left:right])))
	#xrange <- c(0, length(nflx$Open))
	xrange <- c(0, lookatpastndays)
} else {
	yrange <- c(floor(min(nflx$Open)), ceiling(max(nflx$Open)))
	xrange <- c(0, length(nflx$Open))
}

## subset the data based on "left / right" window indexes
data_subset <- nflx$Open[left:right]

rollmean_data <- rollmean(data_subset, rollwindow)
rollvar_data  <- rollapply(data_subset, rollwindow, FUN=var)

mean_var_data <- as.data.frame(cbind(as.matrix(rollmean_data, ncol=1),
                                     as.matrix(rollvar_data,  ncol=1)))

#mean_var_data <- cbind(as.matrix(rollmean_data, ncol=1), as.matrix(rollvar_data, ncol=1))
                               
colnames(mean_var_data) <- c("mean", "var")

jpeg("viz/img%000005d.jpg", width=600, height=600, quality=100)
for (coln in 1:length(mean_var_data$mean)) {
	xrange <- c(floor(min(mean_var_data$mean)), ceiling(max(mean_var_data$mean)))
	yrange <- c(floor(min(mean_var_data$var)),   ceiling(max(mean_var_data$var)))
	if (coln > 1) {
		plot(mean_var_data[1:(coln - 1),], ylim=yrange, xlim=xrange)
		par(new=T)
		plot(mean_var_data[coln, ], ylim=yrange, xlim=xrange, col="red")
	} else {
		plot(mean_var_data[coln, ], ylim=yrange, xlim=xrange, col="red")
	}
}
dev.off()
	

#plot(rollmean_data, xlim=xrange, ylim=yrange, col="darkblue")
#par(new=T)
#plot(nflx$Open[left:right], axes=F, xlab="", ylab="",
#     ylim=yrange, xlim=xrange, cex=0.5, col="darkgreen")
#
#
#
