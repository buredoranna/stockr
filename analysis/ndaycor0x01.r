## this calculates the cor matrix of a port portfolio between
## most recent date (today) and n-days into the past

## read data

tkrs <- c(   "a", "aapl", "adbe", "amat",  "amd",
          "amzn", "brcd", "csco", "ebay",  "emc",
          "goog",  "ibm", "intc", "intu", "jnpr",
          "klac",  "lsi", "mrvl", "msft", "mxim",
          "nflx", "ntap", "nvda", "orcl",  "stx",
          "symc",  "txn",  "wdc", "xlnx", "yhoo")

## generate a series of "read.table" commands
#print("generating read.table list...")
#system("./genreads.bash")
#print("done")

## read in the data sets
#print("reading data...")
#source("readstocks.r")
#print("done")

# open device for jpg output
#if (jpegs == 1) {
#	print("generating images...")
#	system("rm *.jpg")
#	jpeg("img%00004d.jpg", width=800, height=700, quality=100)
#}

cor_over_win <- function(tkr_a, tkr_b, ndaysago) {
	return(cor(tkr_a[1:ndaysago], tkr_b[1:ndaysago]))
}

cor_list <- c()
for (n in 100:3) {
	cor_list <- append(cor_list, cor_over_win(aapl$Open, nflx$Open, n))
}

	
	






