

bond.fund.trade.day.indexes <- tail(trade_day_index_seq, 
                                   length(trade_day_index_seq) - 1)

## weights for how much to have bond fund and how much to be in stock portfolio
bond.index.weight <- 0.5
stock.portfolio.weight <- 1 - bond.index.weight

bond.fund.value <- bond.fund.df[bond.fund.trade.day.indexes, "AdjClose"]
#bond.fund.return <- (bond.fund.return / bond.fund.return[1])

index.fund.value <- index_fund_df[bond.fund.trade.day.indexes, "AdjClose"]
#index.fund

mixed.port.value <- bond.fund.value + index.fund.value
mixed.port.net.return <- (mixed.port.value / mixed.port.value[1]) - 1


#mixed.portfolio.return.df <- cbind(portfolio_return_df, as.data.frame(bond.fund.return))
#colnames(mixed.portfolio.return.df) <- c("Date", "stock.net.return", "stock.gross.return",
#                                         "bond.gross.return")
#
#weighted.return <- stock.portfolio.weight*mixed.portfolio.return.df$stock.gross.return +
#                   bond.index.weight*mixed.portfolio.return.df$bond.gross.return
#
#mixed.portfolio.return.df <- cbind(mixed.portfolio.return.df,
#                                   as.data.frame(weighted.return))
#
#colnames(mixed.portfolio.return.df) <- c("Date", "stock.net.return", "stock.gross.return",
#                                         "bond.gross.return",
#                                         "weighted.gross.return")
#
par(new=T)
plot(mixed.port.net.return, 
     xlim=xrange,
     ylim=yrange,
     type="l",
     col="darkred")

## bond + portfolio ##
average_annual_portfolio_variance <- var(mixed.port.net.return) / years
average_annual_portfolio_stddev   <- sqrt(average_annual_portfolio_variance)

portfolio_return                <- tail(mixed.port.net.return, 1)
average_annual_portfolio_return <- portfolio_return / years
return_percent                  <- round(portfolio_return * 100, 2)
stddev_percent                  <- round(average_annual_portfolio_stddev * 100, 2)
sharpe_ratio                    <- (average_annaul_portfolio_return - one_year_risk_free_rate) /
                                   average_annual_portfolio_stddev
sharpe_ratio                    <- round(sharpe_ratio, 2)

cat(paste("### bond index + vti ###\n"))
cat(paste("return                : ", return_percent, "%\n", sep=""))
cat(paste("average annual return : ", round(return_percent / years, 2), "%\n", sep=""))
cat(paste("annual average std dev: ", stddev_percent, "%\n", sep=""))
cat(paste("sharpe ratio          : ", sharpe_ratio, "\n\n", sep=""))





