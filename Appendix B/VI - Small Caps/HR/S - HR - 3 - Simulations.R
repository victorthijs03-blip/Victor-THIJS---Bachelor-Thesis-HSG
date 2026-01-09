library(MASS)
set.seed(777)

MC  = 10000
Tdays = 63
dt  = 1
nAssets = 13

##### MERTON JUMP INPUTS (DAILY) #####
lambda = 1/252        
muJ    = quantile(SnP600small_HR, 0.01)           
sigmaJ = sd(SnP600small_HR)        

kappa  = exp(muJ + 0.5 * sigmaJ^2) - 1   

##### CORRELATED GAUSSIAN SHOCKS #####
L = t(chol(corr_matrix_S_HR))

Z_all = array(NA, dim = c(MC, nAssets, Tdays))
for (t in 1:Tdays) {
  Z_indep    = matrix(rnorm(MC * nAssets), MC, nAssets)
  Z_all[,,t] = Z_indep %*% L
}

##### STANDARD MERTON JUMP–DIFFUSION MODEL #####
Merton_Model = function(S0, mu_daily, sigma_daily, asset_index) {
  
  path = matrix(NA_real_, nrow = MC, ncol = Tdays + 1)
  path[, 1] = S0
  
  drift = (mu_daily - lambda * kappa - 0.5 * sigma_daily^2) * dt
  
  for (t in 1:Tdays) {
    
    Z_t  = Z_all[, asset_index, t]
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

################ INDEX PART ################
library(quantmod)

getSymbols("^SP600", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")

closing_prices = Cl(SP600)
SnP_600_small  = diff(log(closing_prices))
SnP_600_small  = na.omit(SnP_600_small)

SnP600small_HR = SnP_600_small$SP600.Close[605:1340]
mu_daily_Index    = mean(SnP600small_HR)
sigma_daily_Index = sd(SnP600small_HR)

S0_index = as.numeric(tail(closing_prices)["2025-08-01", ])

Index_Simulation_S_HR =
  Merton_Model(S0_index, mu_daily_Index, sigma_daily_Index, 13)

################ STOCKS PART ################
MUs     = Portfolio_Small_Info$Stock_MU_HR     
SIGMAs  = Portfolio_Small_Info$Stock_Sigma_HR  
SOs     = Portfolio_Small_Info$Stock_So     
Tickers = Portfolio_Small_Info$Stock_Ticker

Price_paths_S_HR = list()
nStocks = nAssets - 1

for (i in 1:nStocks) {
  Price_paths_S_HR[[Tickers[i]]] =
    Merton_Model(SOs[i], MUs[i], SIGMAs[i], i)
}

################ EQUALLY WEIGHTED ################
IndividualWeight = list()
Portfolio_0_S0_S_EW_HR = matrix(0, MC, Tdays + 1)

for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndividualWeight[[ticker]] =
    Price_paths_S_HR[[ticker]] * Table_SEW_HR[i, 3]
  Portfolio_0_S0_S_EW_HR =
    Portfolio_0_S0_S_EW_HR + IndividualWeight[[ticker]]
}

################ VOLATILITY WEIGHTED ################
IndividualWeight = list()
Portfolio_0_S0_S_VW_HR = matrix(0, MC, Tdays + 1)

for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndividualWeight[[ticker]] =
    Price_paths_S_HR[[ticker]] * Table_SVW_HR[i, 3]
  Portfolio_0_S0_S_VW_HR =
    Portfolio_0_S0_S_VW_HR + IndividualWeight[[ticker]]
}

################ INVERSE VOLATILITY WEIGHTED ################
IndividualWeight = list()
Portfolio_0_S0_S_IW_HR = matrix(0, MC, Tdays + 1)

for (i in 1:length(Tickers)) {
  ticker = Tickers[i]
  IndividualWeight[[ticker]] =
    Price_paths_S_HR[[ticker]] * Table_SIW_HR[i, 3]
  Portfolio_0_S0_S_IW_HR =
    Portfolio_0_S0_S_IW_HR + IndividualWeight[[ticker]]
}

################ CORRELATION ################
portfolio_logrets =
  t(apply(Portfolio_0_S0_S_EW_HR, 1, function(x) diff(log(x))))
ETF_logrets =
  t(apply(Index_Simulation_S_HR, 1, function(x) diff(log(x))))

pathwise_corr = sapply(1:nrow(portfolio_logrets), function(i) {
  cor(portfolio_logrets[i, ], ETF_logrets[i, ], use = "complete.obs")
})

list(
  Mean_Correlation   = mean(pathwise_corr, na.rm = TRUE),
  Median_Correlation = median(pathwise_corr, na.rm = TRUE),
  Quantiles          = quantile(pathwise_corr,
                                probs = c(0.05, 0.25, 0.75, 0.95),
                                na.rm = TRUE)
)
