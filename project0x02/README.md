[20141211]

the ideas below about doing a correlation matrix, then choose which stocks based on correlation
aren't quite right.

The idea is sound, but rather than choose stocks based on correlation, the stocks are chosen as
they have been, that is using mean/var, but then the weights (omegas) are determined by the 
correlations

-------

this is a mean/variance portfolio management simulator

so, every n days
	do mean / var analysis
		segment mean/var space into quadrants
		choose stocks in lower right quadrant
	choose stock_1 with highest mean
	
	build correlation matrix
		choose stock_2 with correlation closest to zero with stock_1
	
	sell old stocks
	buy new stocks


[2014????]

startndayago = 1000


subset_stock_symbols

for stock

data.frame <- mydata[[subset_stock_numbers]][meanvar_indexes, "Open"]

stock_price_df <- data.frame()
for (stock_n in subset_meanvar_df$stock_n)
	data_column <- data.frame(mydata[[stock_n]][meanvar_indexes, "Open"])
	colnames(data_column) <- subset_meanvar_df[stock_n, "symbol"]
	stock_price_df <- cbind(stock_price_df, data_column)
}
	


