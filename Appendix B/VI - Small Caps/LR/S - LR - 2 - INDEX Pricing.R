################# INDEX PRICING #######################
### Same reasoning but now for INDEX (S&P 600 Small)

################# Small (S&P 600 Small) + LR ################

rate   = Low_rf
lambda = 1
muJ    = quantile(SnP600small_LR, 0.01)
sigmaJ = sd(SnP600small_LR)

################# BLACK–SCHOLES #################
BS_put_price = function(S, K, T, r, sigma) {
  d1 = (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 = d1 - sigma * sqrt(T)
  
  put_price = K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  return(put_price)
}

################# MERTON PUT #################
Merton_put_price = function(S, K, T, r, sigma, lambda, muJ, sigmaJ, N_max = 20) {
  
  k = exp(muJ + 0.5 * sigmaJ^2) - 1
  put_sum = 0
  
  for (n in 0:N_max) {
    
    lambda_p = lambda * (1 + k)
    pois_wt  = exp(-lambda_p * T) * (lambda_p * T)^n / factorial(n)
    
    sigma_n = sqrt(sigma^2 + (n * sigmaJ^2) / T)
    r_n     = r - lambda * k + (n * muJ) / T
    
    bs_price = BS_put_price(S, K, T, r_n, sigma_n)
    put_sum  = put_sum + pois_wt * bs_price
  }
  
  return(put_sum)
}

################# INDEX HEDGE FUNCTION (NO DELTA) #################

Hedge_INDEX = function(
    sigma, moneyness, So_index, T = 1/4, r,
    contract_size = 100,
    lambda, muJ, sigmaJ, N_max = 20,
    PortfolioBeta, PortfolioValue
) {
  
  K = So_index * moneyness
  put_price = Merton_put_price(
    So_index, K, T, r, sigma, lambda, muJ, sigmaJ, N_max
  )
  
  num_puts = (PortfolioBeta * PortfolioValue) / So_index
  num_contracts = round(num_puts / contract_size, 0)
  
  total_cost = num_contracts * put_price * contract_size
  
  data.frame(
    Spot_Price = round(So_index, 2),
    Strike = round(K, 2),
    Put_Price = round(put_price, 2),
    Premium = round(put_price / So_index, 3),
    Contract_Value = round(put_price * contract_size, 2),
    Num_Contracts = num_contracts,
    Total_Cost = round(total_cost, 2)
  )
}

################# INDEX DATA #################

library(quantmod)

getSymbols("^SP600", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")

closing_prices = Cl(SP600)
SnP_600_small = diff(log(closing_prices))
SnP_600_small = na.omit(SnP_600_small)

SnP600small_LR = SnP_600_small$SP600.Close[605:1340]
sigma = sd(SnP600small_LR) * sqrt(252)

So_index = as.numeric(tail(closing_prices["2025-08-01"]))

################# EW INDEX HEDGE #################

PortfolioValue_S_LR_EW = sum(Table_SEW_LR$Numb_Stocks * Table_SEW_LR$So)
PORT_VAL = PortfolioValue_S_LR_EW
BETA = Beta_S_LR_EW

Hedge_Info_Small_LR_EW <- rbind(
  Hedge_INDEX(sigma, 0.9,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 0.95, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.0,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.05, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
)

rownames(Hedge_Info_Small_LR_EW) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")
Hedge_Info_Small_LR_EW
View(Hedge_Info_Small_LR_EW)


################# Test #################

library(derivmkts)

K  = 0.95 * So_index
T  = 1/4
d  = 0

Merton_put_price(So_index, K, T, rate, sigma, lambda, muJ, sigmaJ, N_max = 40)
mertonjump(So_index, K, sigma, rate, T, d, lambda, muJ, sigmaJ)["Put"]
bsput(So_index, K, sigma, rate, T, d)

################# VW #################

PortfolioValue_S_LR_VW = sum(Table_SVW_LR$Numb_Stocks * Table_SVW_LR$So)
PORT_VAL = PortfolioValue_S_LR_VW
BETA = Beta_S_LR_VW

Hedge_Info_Small_LR_VW <- rbind(
  Hedge_INDEX(sigma, 0.9,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 0.95, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.0,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.05, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
)

rownames(Hedge_Info_Small_LR_VW) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")
Hedge_Info_Small_LR_VW
View(Hedge_Info_Small_LR_VW)


################# IW #################

PortfolioValue_S_LR_IW = sum(Table_SIW_LR$Numb_Stocks * Table_SIW_LR$So)
PORT_VAL = PortfolioValue_S_LR_IW
BETA = Beta_S_LR_IW

Hedge_Info_Small_LR_IW <- rbind(
  Hedge_INDEX(sigma, 0.9,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 0.95, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.0,  So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ),
  Hedge_INDEX(sigma, 1.05, So_index, r = rate, PortfolioBeta = BETA, PortfolioValue = PORT_VAL, lambda = lambda, muJ = muJ, sigmaJ = sigmaJ)
)

rownames(Hedge_Info_Small_LR_IW) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")
Hedge_Info_Small_LR_IW
View(Hedge_Info_Small_LR_IW)
