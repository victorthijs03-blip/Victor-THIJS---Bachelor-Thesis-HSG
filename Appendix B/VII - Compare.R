##### Large 
L_HR =rbind(Compare_L_HR_EW,Compare_L_HR_VW,Compare_L_HR_IW)
View(L_HR)

L_LR =rbind(Compare_L_LR_EW,Compare_L_LR_VW,Compare_L_LR_IW)
View(L_LR)



##### Small 
S_HR =rbind(Compare_S_HR_EW,Compare_S_HR_VW,Compare_S_HR_IW)
View(S_HR)

S_LR =rbind(Compare_S_LR_EW,Compare_S_LR_VW,Compare_S_LR_IW)
View(S_LR)



###final table: 
final_results_table = data.frame(
  Row = c(
    "Hedge Effectiveness",
    "Value",
    "CVaR99",
    "Value",
    "Sharpe Ratio",
    "Value",
    "Mean Terminal Value",
    "Value",
    "Highest Unhedged",
    "Value",
    "Total Hedging Costs",
    "Value"
  ),
  `Large Cap - High Rates` = c(
    "ITM5_IW_Vanilla",
    0.533,
    "ITM5_IW_Vanilla",
    9634848,
    "OTM10_IW_Vanilla",
    0.572,
    "OTM10_VW_Index",
    10935667,
    "VW",
    10987070,
    "ITM5_VW_Vanilla",
    1014274
  ),
  `Large Cap - Low Rates` = c(
    "ITM5_IW_Vanilla",
    0.508,
    "ITM5_IW_Vanilla",
    9541135,
    "OTM10_IW_Vanilla",
    0.664,
    "OTM10_VW_Index",
    11024506,
    "VW",
    11083892,
    "ITM5_VW_Vanilla",
    1093905
  ),
  `Small Cap - High Rates` = c(
    "ITM5_IW_Vanilla",
    0.702,
    "ITM5_IW_Vanilla",
    9548805,
    "ITM5_IW_Vanilla",
    0.019,
    "ITM5_IW_Vanilla",
    10130361,
    "IW",
    10098093,
    "ITM5_VW_Vanilla",
    1125834
  ),
  `Small Cap - Low Rates` = c(
    "ITM5_IW_Vanilla",
    0.548,
    "ITM5_IW_Vanilla",
    9357513,
    "OTM10_IW_Vanilla",
    0.514,
    "OTM10_EW_Index",
    10591928,
    "EW",
    10660308,
    "ITM5_VW_Vanilla",
    1407868
  ),
  stringsAsFactors = FALSE
)

View(final_results_table)
