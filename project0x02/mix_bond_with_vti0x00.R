#years <- 8

#start_ndays_ago <- round(365*years)
#start_index <- ndays_of_stock_data - start_ndays_ago + 1
#last_index <- ndays_of_stock_data

lqd_df <- bond.fund.df
vti_df <- index_fund_df

lqd <- lqd_df$AdjClose[start_index:last_index]
vti <- vti_df$AdjClose[start_index:last_index]

port_value <- 0.5*lqd + 0.5*vti
port_return <- (port_value / port_value[1]) - 1

print("yearly average")
print((tail(port_return, 1) / years) * 100)
print((sd(port_return) / years) * 100)

plot_indexes <- rev(3000 - trade_day_index_seq) + 1
par(new=T)
plot(port_return[plot_indexes], xlim=xrange, ylim=yrange, col="darkred", type="l")

