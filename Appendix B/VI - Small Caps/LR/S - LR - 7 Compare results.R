options(scipen = 999)

Comparing = function(Portfolio, rate_annual, Unhedged, days = 63, total_cost) {
  
  rate_quarter = (1 + rate_annual)^(days/252) - 1 # Convert annual risk-free rate to the 63-day period
  
  Hedged_Terminal   = Portfolio[, 64]  # Extract terminal values
  Unhedged_Terminal = Unhedged[, 64]
  
  Initial_Value = Portfolio[1, 1]
  
  Return  = mean((Hedged_Terminal - Initial_Value) / Initial_Value) # returns over the quarter
  Vol = sd((Hedged_Terminal - Initial_Value) / Initial_Value)
  
  Sharpe = (Return - rate_quarter) / Vol # Quarterly Sharpe Ratio of terminal values
  
  # VaR & CVaR
  VaR_99  = quantile(Hedged_Terminal, 0.01)
  CVaR_99 = mean(Hedged_Terminal[Hedged_Terminal <= VaR_99])
  
  # Hedge effectiveness
  HE = 1 - var(Hedged_Terminal) / var(Unhedged_Terminal)
  
  # Outperformance
  Above = sum(Hedged_Terminal > Unhedged_Terminal)
  
  
  total_cost = total_cost + (1+rate_quarter)
  
  # Output table (1 row only)
  compare_table = data.frame(
    Mean_Terminal_Value   = round(mean(Hedged_Terminal), 0),
    Times_Above_Unhedged  = Above,
    Return     = round(Return, 3),
    Volatility = round(Vol, 3),
    Sharpe_Ratio     = round(Sharpe, 3),
    VaR_99                = round(VaR_99, 0),
    CVaR_99               = round(CVaR_99, 0),
    Hedge_Effectiveness   = round(HE, 3),
    Total_Hedge_Cost = round(total_cost)
  )
  
  rownames(compare_table) = NULL
  
  return(compare_table)
}


########### FOR Equally Weighted #########
Unhedged = Comparing(Portfolio_0_S0_S_EW_LR, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost = 0)
OTM10_EW_Vanilla = Comparing(Portfolio_1_S_EW_LR_OTM10, Low_rf, Portfolio_0_S0_S_EW_LR,  total_cost=total_cost_EW_10)
OTM10_EW_Index   = Comparing(Portfolio_2_S_EW_LR_OTM10, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost= total_cost_EW_10_index)

OTM5_EW_Vanilla  = Comparing(Portfolio_1_S_EW_LR_OTM5, Low_rf, Portfolio_0_S0_S_EW_LR,total_cost=total_cost_EW_5)
OTM5_EW_Index    = Comparing(Portfolio_2_S_EW_LR_OTM5, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost=total_cost_EW_5_index)

ATM0_EW_Vanilla  = Comparing(Portfolio_1_S_EW_LR_ATM0, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost=total_cost_EW_0)
ATM0_EW_Index    = Comparing(Portfolio_2_S_EW_LR_ATM0, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost=total_cost_EW_0_index)

ITM5_EW_Vanilla  = Comparing(Portfolio_1_S_EW_LR_ITM5, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost=total_cost_EW_i5)
ITM5_EW_Index    = Comparing(Portfolio_2_S_EW_LR_ITM5, Low_rf, Portfolio_0_S0_S_EW_LR, total_cost=total_cost_EW_i5_index)

Compare_S_LR_EW = rbind(Unhedged, OTM10_EW_Vanilla, OTM10_EW_Index, OTM5_EW_Vanilla, OTM5_EW_Index, ATM0_EW_Vanilla, ATM0_EW_Index, ITM5_EW_Vanilla, ITM5_EW_Index)
rownames(Compare_S_LR_EW) = c("Unhedged_EW","OTM10_EW_Vanilla", "OTM10_EW_Index", "OTM5_EW_Vanilla", "OTM5_EW_Index", "ATM0_EW_Vanilla", "ATM0_EW_Index","ITM5_EW_Vanilla","ITM5_EW_Index")

View(Compare_S_LR_EW)


########### FOR Volatility Weighted #########
Unhedged = Comparing(Portfolio_0_S0_S_VW_LR, Low_rf, Portfolio_0_S0_S_VW_LR,total_cost=0)
OTM10_VW_Vanilla = Comparing(Portfolio_1_S_VW_LR_OTM10, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_10 )
OTM10_VW_Index   = Comparing(Portfolio_2_S_VW_LR_OTM10, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_10_index)

OTM5_VW_Vanilla  = Comparing(Portfolio_1_S_VW_LR_OTM5, Low_rf, Portfolio_0_S0_S_VW_LR,total_cost=total_cost_VW_5)
OTM5_VW_Index    = Comparing(Portfolio_2_S_VW_LR_OTM5, Low_rf, Portfolio_0_S0_S_VW_LR,total_cost=total_cost_VW_5_index )

ATM0_VW_Vanilla  = Comparing(Portfolio_1_S_VW_LR_ATM0, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_0 )
ATM0_VW_Index    = Comparing(Portfolio_2_S_VW_LR_ATM0, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_0_index)

ITM5_VW_Vanilla  = Comparing(Portfolio_1_S_VW_LR_ITM5, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_i5)
ITM5_VW_Index    = Comparing(Portfolio_2_S_VW_LR_ITM5, Low_rf, Portfolio_0_S0_S_VW_LR, total_cost=total_cost_VW_i5_index)

Compare_S_LR_VW = rbind(Unhedged, OTM10_VW_Vanilla, OTM10_VW_Index, OTM5_VW_Vanilla, OTM5_VW_Index, ATM0_VW_Vanilla, ATM0_VW_Index, ITM5_VW_Vanilla, ITM5_VW_Index)
rownames(Compare_S_LR_VW) = c("Unhedged_VW","OTM10_VW_Vanilla", "OTM10_VW_Index", "OTM5_VW_Vanilla", "OTM5_VW_Index", "ATM0_VW_Vanilla", "ATM0_VW_Index","ITM5_VW_Vanilla","ITM5_VW_Index")

View(Compare_S_LR_VW)


########### FOR Inversely Weighted #########
Unhedged = Comparing(Portfolio_0_S0_S_IW_LR, Low_rf, Portfolio_0_S0_S_IW_LR, total_cost = 0)
OTM10_IW_Vanilla = Comparing(Portfolio_1_S_IW_LR_OTM10, Low_rf, Portfolio_0_S0_S_IW_LR, total_cost=total_cost_IW_10)
OTM10_IW_Index   = Comparing(Portfolio_2_S_IW_LR_OTM10, Low_rf, Portfolio_0_S0_S_IW_LR,total_cost=total_cost_IW_10_index)

OTM5_IW_Vanilla  = Comparing(Portfolio_1_S_IW_LR_OTM5, Low_rf, Portfolio_0_S0_S_IW_LR,total_cost=total_cost_IW_5)
OTM5_IW_Index    = Comparing(Portfolio_2_S_IW_LR_OTM5, Low_rf, Portfolio_0_S0_S_IW_LR,total_cost=total_cost_IW_5_index)

ATM0_IW_Vanilla  = Comparing(Portfolio_1_S_IW_LR_ATM0, Low_rf, Portfolio_0_S0_S_IW_LR,total_cost=total_cost_IW_0)
ATM0_IW_Index    = Comparing(Portfolio_2_S_IW_LR_ATM0, Low_rf, Portfolio_0_S0_S_IW_LR, total_cost=total_cost_IW_0_index)

ITM5_IW_Vanilla  = Comparing(Portfolio_1_S_IW_LR_ITM5, Low_rf, Portfolio_0_S0_S_IW_LR, total_cost=total_cost_IW_i5)
ITM5_IW_Index    = Comparing(Portfolio_2_S_IW_LR_ITM5, Low_rf, Portfolio_0_S0_S_IW_LR, total_cost=total_cost_IW_i5_index)

Compare_S_LR_IW = rbind(Unhedged, OTM10_IW_Vanilla, OTM10_IW_Index, OTM5_IW_Vanilla, OTM5_IW_Index, ATM0_IW_Vanilla, ATM0_IW_Index, ITM5_IW_Vanilla, ITM5_IW_Index)
rownames(Compare_S_LR_IW) = c("Unhedged_IW","OTM10_IW_Vanilla", "OTM10_IW_Index", "OTM5_IW_Vanilla", "OTM5_IW_Index", "ATM0_IW_Vanilla", "ATM0_IW_Index","ITM5_IW_Vanilla","ITM5_IW_Index")

View(Compare_S_LR_IW)
