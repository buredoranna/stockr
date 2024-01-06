foo <- data.frame()
dayn <- trade_day_index_seq[1]

for (s in 1:length(held_tech_stock_symbols)) {
	stock_n <- which(stock_symbols == held_tech_stock_symbols[s])
	cor_window <- ((dayn - nday_meanvar_window):dayn) - 1
	opens <- data.frame(mydata[[stock_n]][cor_window, "Open"])
	colnames(opens) <- c(stock_symbols[stock_n])

	if(s == 1) {
		foo <- cbind(opens)
	} else {
		foo <- cbind(foo, opens)
	}
}

cor_foo <- cor(foo)

uncor_stocks <- which(cor_foo > -0.1 & cor_foo < 0.1, arr.ind=T)
	
