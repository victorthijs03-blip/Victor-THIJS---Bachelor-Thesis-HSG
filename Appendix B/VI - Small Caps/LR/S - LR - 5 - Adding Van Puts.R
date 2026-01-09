########## LR - Small CAP - VANILLA PUT  ##########

simulate_hedged_paths <- function(paths, num_shares, num_hedge_contracts, put_strike, put_price, rate, contract_value = 100, Tdays = 63) {
  
  hedged_paths = paths * num_shares               
  
  terminal_prices = paths[, ncol(paths)]
  
  payoff_per_put  = pmax(put_strike - terminal_prices, 0)
  
  hedge_cost_total = put_price * contract_value * num_hedge_contracts * (1 + rate)^(Tdays / 252)
  
  terminal_value = num_shares * terminal_prices +  contract_value * num_hedge_contracts * payoff_per_put -hedge_cost_total
  
  hedged_paths[, ncol(hedged_paths)] = terminal_value
  
  return(hedged_paths)
}

##########################    EW    ####################################


##### STRIKE INFOS #####
## OTM 10
otm10_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_EW[
    Hedge_Vanilla_S_LR_EW$Moneyness == "OTM_10" &
      Hedge_Vanilla_S_LR_EW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm10_data = t(otm10_data)
colnames(otm10_data) = c("Strike", "Num_Contracts", "Put_Price")

## OTM 5
otm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_EW[
    Hedge_Vanilla_S_LR_EW$Moneyness == "OTM_5" &
      Hedge_Vanilla_S_LR_EW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm5_data = t(otm5_data)
colnames(otm5_data) = c("Strike", "Num_Contracts", "Put_Price")

## ATM 0
atm0_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_EW[
    Hedge_Vanilla_S_LR_EW$Moneyness == "ATM_0" &
      Hedge_Vanilla_S_LR_EW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
atm0_data = t(atm0_data)
colnames(atm0_data) = c("Strike", "Num_Contracts", "Put_Price")

## ITM 5
itm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_EW[
    Hedge_Vanilla_S_LR_EW$Moneyness == "ITM_5" &
      Hedge_Vanilla_S_LR_EW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
itm5_data = t(itm5_data)
colnames(itm5_data) = c("Strike", "Num_Contracts", "Put_Price")


########################## SIMULATIONS ##########################
Tdays = 63

##### OTM 10 #####
Portfolio_1_S_EW_LR_OTM10 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_EW_LR_OTM10 = Portfolio_1_S_EW_LR_OTM10 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SEW_LR[i,3], 
                          num_hedge_contracts=otm10_data[i,2], 
                          put_strike=otm10_data[i,1], 
                          put_price=otm10_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_EW_LR_OTM10[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - EW - OTM 10")
abline(h=Portfolio_1_S_EW_LR_OTM10[1,1], col="red", lwd=2, lty=2)

total_cost_EW_10 = sum(otm10_data[,2]*otm10_data[,3]*100)
mean_EW_10 = mean(Portfolio_1_S_EW_LR_OTM10[,Tdays+1])
min_EW_10 = min(Portfolio_1_S_EW_LR_OTM10[,Tdays+1])


##### OTM 5 #####
Portfolio_1_S_EW_LR_OTM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_EW_LR_OTM5 = Portfolio_1_S_EW_LR_OTM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SEW_LR[i,3], 
                          num_hedge_contracts=otm5_data[i,2], 
                          put_strike=otm5_data[i,1], 
                          put_price=otm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_EW_LR_OTM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - EW - OTM 5")
abline(h=Portfolio_1_S_EW_LR_OTM5[1,1], col="red", lwd=2, lty=2)

total_cost_EW_5 = sum(otm5_data[,2]*otm5_data[,3]*100)
mean_EW_5 = mean(Portfolio_1_S_EW_LR_OTM5[,Tdays+1])
min_EW_5 = min(Portfolio_1_S_EW_LR_OTM5[,Tdays+1])


##### ATM 0 #####
Portfolio_1_S_EW_LR_ATM0 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_EW_LR_ATM0 = Portfolio_1_S_EW_LR_ATM0 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SEW_LR[i,3], 
                          num_hedge_contracts=atm0_data[i,2], 
                          put_strike=atm0_data[i,1], 
                          put_price=atm0_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_EW_LR_ATM0[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - EW - ATM 0")
abline(h=Portfolio_1_S_EW_LR_ATM0[1,1], col="red", lwd=2, lty=2)

total_cost_EW_0 = sum(atm0_data[,2]*atm0_data[,3]*100)
mean_EW_0 = mean(Portfolio_1_S_EW_LR_ATM0[,Tdays+1])
min_EW_0 = min(Portfolio_1_S_EW_LR_ATM0[,Tdays+1])


##### ITM 5 #####
Portfolio_1_S_EW_LR_ITM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_EW_LR_ITM5 = Portfolio_1_S_EW_LR_ITM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SEW_LR[i,3], 
                          num_hedge_contracts=itm5_data[i,2], 
                          put_strike=itm5_data[i,1], 
                          put_price=itm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_EW_LR_ITM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - EW - ITM 5")
abline(h=Portfolio_1_S_EW_LR_ITM5[1,1], col="red", lwd=2, lty=2)

total_cost_EW_i5 = sum(itm5_data[,2]*itm5_data[,3]*100)
mean_EW_i5 = mean(Portfolio_1_S_EW_LR_ITM5[,Tdays+1])
min_EW_i5 = min(Portfolio_1_S_EW_LR_ITM5[,Tdays+1])


##########################    VW    ####################################

##### STRIKE INFOS #####
## OTM 10
otm10_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_VW[
    Hedge_Vanilla_S_LR_VW$Moneyness == "OTM_10" &
      Hedge_Vanilla_S_LR_VW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm10_data = t(otm10_data)
colnames(otm10_data) = c("Strike", "Num_Contracts", "Put_Price")

## OTM 5
otm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_VW[
    Hedge_Vanilla_S_LR_VW$Moneyness == "OTM_5" &
      Hedge_Vanilla_S_LR_VW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm5_data = t(otm5_data)
colnames(otm5_data) = c("Strike", "Num_Contracts", "Put_Price")

## ATM 0
atm0_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_VW[
    Hedge_Vanilla_S_LR_VW$Moneyness == "ATM_0" &
      Hedge_Vanilla_S_LR_VW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
atm0_data = t(atm0_data)
colnames(atm0_data) = c("Strike", "Num_Contracts", "Put_Price")

## ITM 5
itm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_VW[
    Hedge_Vanilla_S_LR_VW$Moneyness == "ITM_5" &
      Hedge_Vanilla_S_LR_VW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
itm5_data = t(itm5_data)
colnames(itm5_data) = c("Strike", "Num_Contracts", "Put_Price")


########################## SIMULATIONS ##########################
Tdays = 63

##### OTM 10 #####
Portfolio_1_S_VW_LR_OTM10 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_VW_LR_OTM10 = Portfolio_1_S_VW_LR_OTM10 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SVW_LR[i,3], 
                          num_hedge_contracts=otm10_data[i,2], 
                          put_strike=otm10_data[i,1], 
                          put_price=otm10_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_VW_LR_OTM10[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - VW - OTM 10")
abline(h=Portfolio_1_S_VW_LR_OTM10[1,1], col="red", lwd=2, lty=2)

total_cost_VW_10 = sum(otm10_data[,2]*otm10_data[,3]*100)
mean_VW_10 = mean(Portfolio_1_S_VW_LR_OTM10[,Tdays+1])
min_VW_10 = min(Portfolio_1_S_VW_LR_OTM10[,Tdays+1])


##### OTM 5 #####
Portfolio_1_S_VW_LR_OTM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_VW_LR_OTM5 = Portfolio_1_S_VW_LR_OTM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SVW_LR[i,3], 
                          num_hedge_contracts=otm5_data[i,2], 
                          put_strike=otm5_data[i,1], 
                          put_price=otm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_VW_LR_OTM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - VW - OTM 5")
abline(h=Portfolio_1_S_VW_LR_OTM5[1,1], col="red", lwd=2, lty=2)

total_cost_VW_5 = sum(otm5_data[,2]*otm5_data[,3]*100)
mean_VW_5 = mean(Portfolio_1_S_VW_LR_OTM5[,Tdays+1])
min_VW_5 = min(Portfolio_1_S_VW_LR_OTM5[,Tdays+1])


##### ATM 0 #####
Portfolio_1_S_VW_LR_ATM0 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_VW_LR_ATM0 = Portfolio_1_S_VW_LR_ATM0 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SVW_LR[i,3], 
                          num_hedge_contracts=atm0_data[i,2], 
                          put_strike=atm0_data[i,1], 
                          put_price=atm0_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_VW_LR_ATM0[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - VW - ATM 0")
abline(h=Portfolio_1_S_VW_LR_ATM0[1,1], col="red", lwd=2, lty=2)

total_cost_VW_0 = sum(atm0_data[,2]*atm0_data[,3]*100)
mean_VW_0 = mean(Portfolio_1_S_VW_LR_ATM0[,Tdays+1])
min_VW_0 = min(Portfolio_1_S_VW_LR_ATM0[,Tdays+1])


##### ITM 5 #####
Portfolio_1_S_VW_LR_ITM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_VW_LR_ITM5 = Portfolio_1_S_VW_LR_ITM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SVW_LR[i,3], 
                          num_hedge_contracts=itm5_data[i,2], 
                          put_strike=itm5_data[i,1], 
                          put_price=itm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_VW_LR_ITM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - VW - ITM 5")
abline(h=Portfolio_1_S_VW_LR_ITM5[1,1], col="red", lwd=2, lty=2)

total_cost_VW_i5 = sum(itm5_data[,2]*itm5_data[,3]*100)
mean_VW_i5 = mean(Portfolio_1_S_VW_LR_ITM5[,Tdays+1])
min_VW_i5 = min(Portfolio_1_S_VW_LR_ITM5[,Tdays+1])

##########################    IW    ####################################

##### STRIKE INFOS #####
## OTM 10
otm10_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_IW[
    Hedge_Vanilla_S_LR_IW$Moneyness == "OTM_10" &
      Hedge_Vanilla_S_LR_IW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm10_data = t(otm10_data)
colnames(otm10_data) = c("Strike", "Num_Contracts", "Put_Price")

## OTM 5
otm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_IW[
    Hedge_Vanilla_S_LR_IW$Moneyness == "OTM_5" &
      Hedge_Vanilla_S_LR_IW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
otm5_data = t(otm5_data)
colnames(otm5_data) = c("Strike", "Num_Contracts", "Put_Price")

## ATM 0
atm0_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_IW[
    Hedge_Vanilla_S_LR_IW$Moneyness == "ATM_0" &
      Hedge_Vanilla_S_LR_IW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
atm0_data = t(atm0_data)
colnames(atm0_data) = c("Strike", "Num_Contracts", "Put_Price")

## ITM 5
itm5_data = sapply(Tickers, function(ticker) {
  row = Hedge_Vanilla_S_LR_IW[
    Hedge_Vanilla_S_LR_IW$Moneyness == "ITM_5" &
      Hedge_Vanilla_S_LR_IW$Ticker == ticker, ]
  c(row$Strike, row$Num_Contracts, row$Put_Price)
})
itm5_data = t(itm5_data)
colnames(itm5_data) = c("Strike", "Num_Contracts", "Put_Price")


########################## SIMULATIONS ##########################
Tdays = 63

##### OTM 10 #####
Portfolio_1_S_IW_LR_OTM10 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_IW_LR_OTM10 = Portfolio_1_S_IW_LR_OTM10 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SIW_LR[i,3], 
                          num_hedge_contracts=otm10_data[i,2], 
                          put_strike=otm10_data[i,1], 
                          put_price=otm10_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_IW_LR_OTM10[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - IW - OTM 10")
abline(h=Portfolio_1_S_IW_LR_OTM10[1,1], col="red", lwd=2, lty=2)

total_cost_IW_10 = sum(otm10_data[,2]*otm10_data[,3]*100)
mean_IW_10 = mean(Portfolio_1_S_IW_LR_OTM10[,Tdays+1])
min_IW_10 = min(Portfolio_1_S_IW_LR_OTM10[,Tdays+1])


##### OTM 5 #####
Portfolio_1_S_IW_LR_OTM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_IW_LR_OTM5 = Portfolio_1_S_IW_LR_OTM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SIW_LR[i,3], 
                          num_hedge_contracts=otm5_data[i,2], 
                          put_strike=otm5_data[i,1], 
                          put_price=otm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_IW_LR_OTM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - IW - OTM 5")
abline(h=Portfolio_1_S_IW_LR_OTM5[1,1], col="red", lwd=2, lty=2)

total_cost_IW_5 = sum(otm5_data[,2]*otm5_data[,3]*100)
mean_IW_5 = mean(Portfolio_1_S_IW_LR_OTM5[,Tdays+1])
min_IW_5 = min(Portfolio_1_S_IW_LR_OTM5[,Tdays+1])


##### ATM 0 #####
Portfolio_1_S_IW_LR_ATM0 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_IW_LR_ATM0 = Portfolio_1_S_IW_LR_ATM0 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SIW_LR[i,3], 
                          num_hedge_contracts=atm0_data[i,2], 
                          put_strike=atm0_data[i,1], 
                          put_price=atm0_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_IW_LR_ATM0[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - IW - ATM 0")
abline(h=Portfolio_1_S_IW_LR_ATM0[1,1], col="red", lwd=2, lty=2)

total_cost_IW_0 = sum(atm0_data[,2]*atm0_data[,3]*100)
mean_IW_0 = mean(Portfolio_1_S_IW_LR_ATM0[,Tdays+1])
min_IW_0 = min(Portfolio_1_S_IW_LR_ATM0[,Tdays+1])


##### ITM 5 #####
Portfolio_1_S_IW_LR_ITM5 = matrix(0, nrow=MC, ncol=Tdays+1)
for (i in 1:length(Tickers)) {
  Portfolio_1_S_IW_LR_ITM5 = Portfolio_1_S_IW_LR_ITM5 + 
    simulate_hedged_paths(Price_paths_S_LR[[Tickers[i]]], 
                          num_shares=Table_SIW_LR[i,3], 
                          num_hedge_contracts=itm5_data[i,2], 
                          put_strike=itm5_data[i,1], 
                          put_price=itm5_data[i,3], 
                          rate=Low_rf)
}

matplot(t(Portfolio_1_S_IW_LR_ITM5[1:10,]), type="l", lty=1, col="black",
        xlab="Time Step", ylab="Portfolio Value",
        main="First 10 Simulated Portfolio Paths Van. - S - SR - IW - ITM 5")
abline(h=Portfolio_1_S_IW_LR_ITM5[1,1], col="red", lwd=2, lty=2)

total_cost_IW_i5 = sum(itm5_data[,2]*itm5_data[,3]*100)
mean_IW_i5 = mean(Portfolio_1_S_IW_LR_ITM5[,Tdays+1])
min_IW_i5 = min(Portfolio_1_S_IW_LR_ITM5[,Tdays+1])


