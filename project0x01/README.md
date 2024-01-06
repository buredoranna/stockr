BASELINE:
	sp500 index fund

FIRST:
	setup a "mean / variance" system as a "managed" baseline for comparison 
	I need a system which does mean-variance analysis and purchasing to compare "my" system too.

So, a mean variance analysis of a bunch of stocks, pick the top three securities with the highest mean, and lowest variance:

start on date DATE
	use past n_days_in_analysis_window for analysis (30 / 60 / 90)
	mean_first = T/F
	var_first = T/F
	while (number of days left < trade_every_n_days) {
		calculate mean over n_days_in_analysis_window
			sort by mean
			choose top stocks
				calculate variance of chosen stocks over n_days_in_analysis_window
		
		sort by variance
		choose top V
	}

the idea here,
	is to fit a model,
	choose the model with the best fit,
	use the model to do an n-day forecast
	choose the stock witht "best" forecast: by way of mean / variance analysis
	and see how those results compare to sp500
	
