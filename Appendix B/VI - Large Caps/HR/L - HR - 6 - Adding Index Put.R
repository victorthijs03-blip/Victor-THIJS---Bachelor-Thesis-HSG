###### INDEX PUT HEDGING 
##### L - HR 

rate = High_rf

#Net payoff function 
net_value_index = function(St, K, Put_Price, NumberContracts, rate) {
  profit_per_index_share = pmax(K - St, 0) - Put_Price * (1 + rate)^(63/252)
  profit_with_index = profit_per_index_share * NumberContracts * 100
  return(profit_with_index)
}

Index_Put_Hedge = function(Index_paths, Strike, rate,Portfolio_to_be_Hedged, Hedge_Info) {
  St = Index_paths[, 64]
  K = Hedge_Info[Strike, "Strike"]
  Put_Price = Hedge_Info[Strike, "Put_Price"]
  NumberContracts = Hedge_Info[Strike, "Num_Contracts"]
  
  Put_payoff = net_value_index(St, K, Put_Price, NumberContracts, rate)
  
  Hedged_Portfolio = Portfolio_to_be_Hedged
  Hedged_Portfolio[, 64] = Portfolio_to_be_Hedged[, 64] + Put_payoff
  return(Hedged_Portfolio)
}


############### EQUALLY WEIGHTED ###################


######## OTM 10 ########
Portfolio_2_L_EW_HR_OTM10 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_10", High_rf,
  Portfolio_0_S0_L_EW_HR, Hedge_Info_Large_HR_EW
)

matplot(t(Portfolio_2_L_EW_HR_OTM10[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - EW - OTM 10")
abline(h = Portfolio_2_L_EW_HR_OTM10[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_EW_10_index = Hedge_Info_Large_HR_EW["OTM_10","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_EW["OTM_10","Num_Contracts"] * 100

mean(Portfolio_2_L_EW_HR_OTM10[,64])
min(Portfolio_2_L_EW_HR_OTM10[,64])


######## OTM 5 ########
Portfolio_2_L_EW_HR_OTM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_5", High_rf,
  Portfolio_0_S0_L_EW_HR, Hedge_Info_Large_HR_EW
)

matplot(t(Portfolio_2_L_EW_HR_OTM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - EW - OTM 5")
abline(h = Portfolio_2_L_EW_HR_OTM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_EW_5_index = Hedge_Info_Large_HR_EW["OTM_5","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_EW["OTM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_EW_HR_OTM5[,64])
min(Portfolio_2_L_EW_HR_OTM5[,64])


######## ATM 0 ########
Portfolio_2_L_EW_HR_ATM0 = Index_Put_Hedge(Index_Simulation_L_HR, "ATM_0", High_rf, Portfolio_0_S0_L_EW_HR, Hedge_Info_Large_HR_EW)

matplot(t(Portfolio_2_L_EW_HR_ATM0[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - EW - ATM")
abline(h = Portfolio_2_L_EW_HR_ATM0[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_EW_0_index = Hedge_Info_Large_HR_EW["ATM_0","Put_Price"] *(1 + rate)^(63/252) * Hedge_Info_Large_HR_EW["ATM_0","Num_Contracts"] * 100

mean(Portfolio_2_L_EW_HR_ATM0[,64])
min(Portfolio_2_L_EW_HR_ATM0[,64])


######## ITM 5 ########
Portfolio_2_L_EW_HR_ITM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "ITM_5", High_rf,
  Portfolio_0_S0_L_EW_HR, Hedge_Info_Large_HR_EW
)

matplot(t(Portfolio_2_L_EW_HR_ITM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - EW - ITM 5")
abline(h = Portfolio_2_L_EW_HR_ITM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_EW_i5_index = Hedge_Info_Large_HR_EW["ITM_5","Put_Price"] * (1 + rate)^(63/252) * Hedge_Info_Large_HR_EW["ITM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_EW_HR_ITM5[,64])
min(Portfolio_2_L_EW_HR_ITM5[,64])


############### VOLATILITY WEIGHTED ################

######## OTM 10 ########
Portfolio_2_L_VW_HR_OTM10 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_10", High_rf,
  Portfolio_0_S0_L_VW_HR, Hedge_Info_Large_HR_VW
)

matplot(t(Portfolio_2_L_VW_HR_OTM10[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - VW - OTM 10")
abline(h = Portfolio_2_L_VW_HR_OTM10[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_VW_10_index = Hedge_Info_Large_HR_VW["OTM_10","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_VW["OTM_10","Num_Contracts"] * 100

mean(Portfolio_2_L_VW_HR_OTM10[,64])
min(Portfolio_2_L_VW_HR_OTM10[,64])


######## OTM 5 ########
Portfolio_2_L_VW_HR_OTM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_5", High_rf,
  Portfolio_0_S0_L_VW_HR, Hedge_Info_Large_HR_VW
)

matplot(t(Portfolio_2_L_VW_HR_OTM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - VW - OTM 5")
abline(h = Portfolio_2_L_VW_HR_OTM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_VW_5_index = Hedge_Info_Large_HR_VW["OTM_5","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_VW["OTM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_VW_HR_OTM5[,64])
min(Portfolio_2_L_VW_HR_OTM5[,64])


######## ATM 0 ########
Portfolio_2_L_VW_HR_ATM0 = Index_Put_Hedge(
  Index_Simulation_L_HR, "ATM_0", High_rf,
  Portfolio_0_S0_L_VW_HR, Hedge_Info_Large_HR_VW
)

matplot(t(Portfolio_2_L_VW_HR_ATM0[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - VW - ATM")
abline(h = Portfolio_2_L_VW_HR_ATM0[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_VW_0_index = Hedge_Info_Large_HR_VW["ATM_0","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_VW["ATM_0","Num_Contracts"] * 100

mean(Portfolio_2_L_VW_HR_ATM0[,64])
min(Portfolio_2_L_VW_HR_ATM0[,64])


######## ITM 5 ########
Portfolio_2_L_VW_HR_ITM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "ITM_5", High_rf,
  Portfolio_0_S0_L_VW_HR, Hedge_Info_Large_HR_VW
)

matplot(t(Portfolio_2_L_VW_HR_ITM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - VW - ITM 5")
abline(h = Portfolio_2_L_VW_HR_ITM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_VW_i5_index = Hedge_Info_Large_HR_VW["ITM_5","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_VW["ITM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_VW_HR_ITM5[,64])
min(Portfolio_2_L_VW_HR_ITM5[,64])


########## INVERSE VOLATILITY WEIGHTED #############



######## OTM 10 ########
Portfolio_2_L_IW_HR_OTM10 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_10", High_rf,
  Portfolio_0_S0_L_IW_HR, Hedge_Info_Large_HR_IW
)

matplot(t(Portfolio_2_L_IW_HR_OTM10[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - IW - OTM 10")
abline(h = Portfolio_2_L_IW_HR_OTM10[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_IW_10_index = Hedge_Info_Large_HR_IW["OTM_10","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_IW["OTM_10","Num_Contracts"] * 100

mean(Portfolio_2_L_IW_HR_OTM10[,64])
min(Portfolio_2_L_IW_HR_OTM10[,64])


######## OTM 5 ########
Portfolio_2_L_IW_HR_OTM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "OTM_5", High_rf,
  Portfolio_0_S0_L_IW_HR, Hedge_Info_Large_HR_IW
)

matplot(t(Portfolio_2_L_IW_HR_OTM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - IW - OTM 5")
abline(h = Portfolio_2_L_IW_HR_OTM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_IW_5_index = Hedge_Info_Large_HR_IW["OTM_5","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_IW["OTM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_IW_HR_OTM5[,64])
min(Portfolio_2_L_IW_HR_OTM5[,64])


######## ATM 0 ########
Portfolio_2_L_IW_HR_ATM0 = Index_Put_Hedge(
  Index_Simulation_L_HR, "ATM_0", High_rf,
  Portfolio_0_S0_L_IW_HR, Hedge_Info_Large_HR_IW
)

matplot(t(Portfolio_2_L_IW_HR_ATM0[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - IW - ATM")
abline(h = Portfolio_2_L_IW_HR_ATM0[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_IW_0_index = Hedge_Info_Large_HR_IW["ATM_0","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_IW["ATM_0","Num_Contracts"] * 100

mean(Portfolio_2_L_IW_HR_ATM0[,64])
min(Portfolio_2_L_IW_HR_ATM0[,64])


######## ITM 5 ########
Portfolio_2_L_IW_HR_ITM5 = Index_Put_Hedge(
  Index_Simulation_L_HR, "ITM_5", High_rf,
  Portfolio_0_S0_L_IW_HR, Hedge_Info_Large_HR_IW
)

matplot(t(Portfolio_2_L_IW_HR_ITM5[1:50,]),
        type = "l", lty = 1, col = "black",
        xlab = "Time Step", ylab = "Portfolio Value",
        main = "Index Hedged Portfolio - IW - ITM 5")
abline(h = Portfolio_2_L_IW_HR_ITM5[1,1],
       col = "red", lwd = 2, lty = 2)

total_cost_IW_i5_index = Hedge_Info_Large_HR_IW["ITM_5","Put_Price"] *
  (1 + rate)^(63/252) *
  Hedge_Info_Large_HR_IW["ITM_5","Num_Contracts"] * 100

mean(Portfolio_2_L_IW_HR_ITM5[,64])
min(Portfolio_2_L_IW_HR_ITM5[,64])



       
