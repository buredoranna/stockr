# read in stock data
aapl <- read.table("/home/user/data/stocks/aapl.csv", sep=",", header=T)
goog <- read.table("/home/user/data/stocks/goog.csv", sep=",", header=T)
orcl <- read.table("/home/user/data/stocks/orcl.csv", sep=",", header=T)

# this must exist to be referenced later
# end-date, correlation
cors <- c()
covs <- c()
dates <- c()

# number of days in correlation window
daysperset <- 5

flr <- 2
for (s in 1:700) {
	# ceil <- s
	# flr <- s + 5
	# cors[s] <- cor(aapl$Close[ceil:flr], goog$Close[ceil:flr])
	cors[s] <- cor(aapl$Close[1:flr], goog$Close[1:flr])
	covs[s] <- cor(aapl$Close[1:flr], goog$Close[1:flr])
	flr <- flr + 1
}
