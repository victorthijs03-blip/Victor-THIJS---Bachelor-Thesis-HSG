library(MASS)
set.seed(777)

MC  = 10000
Tdays = 63      # 63 simulation days + initial S0
dt  = 1         # 1 day
nAssets = 13

##### MERTON JUMP INPUTS (DAILY) #####
lambda = 1/252        
muJ    = quantile(SnP_500_LR, 0.01)           
sigmaJ = sd(SnP_500_LR)        
kappa  = exp(muJ + 0.5 * sigmaJ^2) - 1   

##### CORRELATED GAUSSIAN SHOCKS #####
L = t(chol(corr_matrix_L_LR))            # nAssets x nAssets
Z_all = array(NA, dim = c(MC, nAssets, Tdays))
for (t in 1:Tdays) {
  Z_indep = matrix(rnorm(MC * nAssets), MC, nAssets)
  Z_all[,,t] = Z_indep %*% L             
}

##### STANDARD MERTON JUMP-DIFFUSION MODEL #####
Merton_Model = function(S0, mu_daily, sigma_daily, asset_index) {
  path = matrix(NA_real_, nrow = MC, ncol = Tdays + 1)
  path[, 1] = S0
  drift = (mu_daily - lambda * kappa - 0.5 * sigma_daily^2) * dt
  for (t in 1:Tdays) {
    Z_t = Z_all[, asset_index, t]
    diff = sigma_daily * sqrt(dt) * Z_t
    N_jump = rpois(MC, lambda * dt)    
    logJump = numeric(MC)
    idx = which(N_jump > 0)
    if (length(idx) > 0) {
      for (i in idx) {
        logY = muJ + sigmaJ * rnorm(N_jump[i])
        logJump[i] = sum(logY)
      }
    }
    path[, t + 1] = path[, t] * exp(drift + diff + logJump)
  }
  return(path)
}

#### Index part
library(quantmod)
getSymbols("^GSPC", src = "yahoo", from = "2020-04-01", to = "2025-08-02")
closing_prices = Cl(GSPC)
SnP_500 = diff(log(closing_prices))
SnP_500 = na.omit(SnP_500)
SnP_500_LR = SnP_500$GSPC.Close[1:604]
mu_daily_Index <- mean(SnP_500_LR)
sigma_daily_Index <- sd(SnP_500_LR)
S0_index = as.numeric(tail(closing_prices)["2025-08-01", ])
Index_Simulation_L_LR = Merton_Model(S0_index, mu_daily_Index, sigma_daily_Index, 13)

#### Stocks part
MUs = Portfolio_Large_Info$Stock_MU_LR     
SIGMAs = Portfolio_Large_Info$Stock_Sigma_LR  
SOs = Portfolio_Large_Info$Stock_So     
Tickers = Portfolio_Large_Info$Stock_Ticker

Price_paths_L_LR = list()
nStocks = nAssets - 1
for (i in 1:nStocks) {
  indiv_price_paths = Merton_Model(S0 = SOs[i], mu_daily = MUs[i], sigma_daily = SIGMAs[i], asset_index = i)
  Price_paths_L_LR[[Tickers[i]]] = indiv_price_paths
}

####################### EQUALLY WEIGHTED PORTFOLIO #######################
IndivualWeigth = list()
Portfolio_0_S0_L_EW_LR = matrix(0, nrow = MC, ncol = Tdays + 1)
for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndivualWeigth[[ticker]] = Price_paths_L_LR[[ticker]] * Table_LEW_LR[i,3]
  Portfolio_0_S0_L_EW_LR = Portfolio_0_S0_L_EW_LR + IndivualWeigth[[ticker]]
}

####################### VOLATILITY WEIGHTED PORTFOLIO #######################
IndivualWeigth = list()
Portfolio_0_S0_L_VW_LR = matrix(0, nrow = MC, ncol = Tdays + 1)
for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndivualWeigth[[ticker]] = Price_paths_L_LR[[ticker]] * Table_LVW_LR[i,3]
  Portfolio_0_S0_L_VW_LR = Portfolio_0_S0_L_VW_LR + IndivualWeigth[[ticker]]
}

####################### INVERSLY VOLATILITY WEIGHTED PORTFOLIO ####################### 
IndivualWeigth = list()
Portfolio_0_S0_L_IW_LR = matrix(0, nrow = MC, ncol = Tdays + 1)
for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndivualWeigth[[ticker]] = Price_paths_L_LR[[ticker]] * Table_LIW_LR[i,3]
  Portfolio_0_S0_L_IW_LR = Portfolio_0_S0_L_IW_LR + IndivualWeigth[[ticker]]
}

################ CORRELATION ################
portfolio_logrets <- t(apply(Portfolio_0_S0_L_EW_LR, 1, function(x) diff(log(x))))
index_logrets <- t(apply(Index_Simulation_L_LR, 1, function(x) diff(log(x))))

pathwise_corr <- sapply(1:nrow(portfolio_logrets), function(i) {
  cor(portfolio_logrets[i, ], index_logrets[i, ], use = "complete.obs")
})

list(
  Mean_Correlation = mean(pathwise_corr, na.rm = TRUE),
  Median_Correlation = median(pathwise_corr, na.rm = TRUE),
  Quantiles = quantile(pathwise_corr, probs = c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)
)
