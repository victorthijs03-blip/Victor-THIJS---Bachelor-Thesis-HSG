################# Large (S&P 500) + LR ###############

rate   = Low_rf
lambda = 1
muJ    = quantile(SnP_500_LR, 0.01)
sigmaJ = sd(SnP_500_LR)

################# BLACK–SCHOLES #################

BS_put_price = function(S, K, T, r, sigma) {
  d1 = (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 = d1 - sigma * sqrt(T)
  put_price = K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  return(put_price)
}

################# MERTON JUMP–DIFFUSION PUT #################

Merton_put_price = function(S, K, T, r, sigma, lambda, muJ, sigmaJ, N_max = 20) {
  k = exp(muJ + 0.5 * sigmaJ^2) - 1
  put_sum = 0
  for (n in 0:N_max) {
    lambda_p = lambda * (1 + k)
    pois_wt  = exp(-lambda_p * T) * (lambda_p * T)^n / factorial(n)
    sigma_n  = sqrt(sigma^2 + (n * sigmaJ^2) / T)
    r_n      = r - lambda * k + (n * muJ) / T
    bs_price = BS_put_price(S, K, T, r_n, sigma_n)
    put_sum  = put_sum + pois_wt * bs_price
  }
  return(put_sum)
}

################# FINAL HEDGE COST FUNCTION #################

Hedge_Vanilla = function(sigma, moneyness, S, T = 1/4, r, N_shares, contract_size = 100, lambda, muJ, sigmaJ, N_max = 20) {
  K = S * moneyness
  put_price = Merton_put_price(S, K, T, r, sigma, lambda, muJ, sigmaJ, N_max)
  num_contracts = round(N_shares / contract_size, 0)
  total_cost = num_contracts * put_price * contract_size
  data.frame(
    Spot_Price = round(S, 2),
    Strike = round(K, 2),
    Put_Price = round(put_price, 2),
    Premium = round(put_price / S, 3),
    Number_ofShares = N_shares,
    Contract_Value = round(put_price * contract_size, 2),
    Num_Contracts = num_contracts,
    Total_Cost = round(total_cost, 2)
  )
}

#################################### EW #####################################

Hedge_Vanilla_L_LR_EW = list()

moneyness_levels = c(0.9, 0.95, 1, 1.05)
names(moneyness_levels) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")

SIGMAs  = Portfolio_Large_Info$Stock_Sigma_LR * sqrt(252)
SOs     = Portfolio_Large_Info$Stock_So
Tickers = Portfolio_Large_Info$Stock_Ticker

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks = Table_LEW_LR$Numb_Stocks[i]
  for (m in moneyness_levels) {
    result = Hedge_Vanilla(SIGMAs[i], m, SOs[i], r = rate, N_shares = Number_of_Stocks, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  stock_hedges$Ticker = Tickers[i]
  Hedge_Vanilla_L_LR_EW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_L_LR_EW = do.call(rbind, Hedge_Vanilla_L_LR_EW)
rownames(Hedge_Vanilla_L_LR_EW) = NULL
Hedge_Vanilla_L_LR_EW
View(Hedge_Vanilla_L_LR_EW)

#################################### VW #####################################

Hedge_Vanilla_L_LR_VW = list()

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks = Table_LVW_LR$Numb_Stocks[i]
  for (m in moneyness_levels) {
    result = Hedge_Vanilla(SIGMAs[i], m, SOs[i], r = rate, N_shares = Number_of_Stocks, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  stock_hedges$Ticker = Tickers[i]
  Hedge_Vanilla_L_LR_VW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_L_LR_VW = do.call(rbind, Hedge_Vanilla_L_LR_VW)
rownames(Hedge_Vanilla_L_LR_VW) = NULL
Hedge_Vanilla_L_LR_VW
View(Hedge_Vanilla_L_LR_VW)

#################################### IW #####################################

Hedge_Vanilla_L_LR_IW = list()

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks = Table_LIW_LR$Numb_Stocks[i]
  for (m in moneyness_levels) {
    result = Hedge_Vanilla(SIGMAs[i], m, SOs[i], r = rate, N_shares = Number_of_Stocks, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  stock_hedges$Ticker = Tickers[i]
  Hedge_Vanilla_L_LR_IW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_L_LR_IW = do.call(rbind, Hedge_Vanilla_L_LR_IW)
rownames(Hedge_Vanilla_L_LR_IW) = NULL
Hedge_Vanilla_L_LR_IW
View(Hedge_Vanilla_L_LR_IW)