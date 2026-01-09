################# Small (S&P 600 Small) + HR ###############

rate   = High_rf
lambda = 1
muJ    = quantile(SnP600small_HR, 0.01)
sigmaJ = sd(SnP600small_HR)

## Hence k of around (for discussion)
kappa = exp(muJ + 0.5 * sigmaJ^2) - 1
kappa

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
    
    sigma_n = sqrt(sigma^2 + (n * sigmaJ^2) / T)
    r_n     = r - lambda * k + (n * muJ) / T
    
    bs_price = BS_put_price(S, K, T, r_n, sigma_n)
    put_sum  = put_sum + pois_wt * bs_price
  }
  
  return(put_sum)
}

################# FINAL HEDGE COST FUNCTION #################

Hedge_Vanilla = function(
    sigma, moneyness, S, T = 1/4, r,
    N_shares, contract_size = 100,
    lambda, muJ, sigmaJ, N_max = 20
) {
  
  K = S * moneyness
  put_price = Merton_put_price(S, K, T, r, sigma, lambda, muJ, sigmaJ, N_max)
  
  num_contracts = round(N_shares / contract_size, 0)
  total_cost   = num_contracts * put_price * contract_size
  
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

Hedge_Vanilla_S_HR_EW = list()

moneyness_levels = c(0.9, 0.95, 1, 1.05)
names(moneyness_levels) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")

SIGMAs  = Portfolio_Small_Info$Stock_Sigma_HR
SIGMAs  = SIGMAs * sqrt(252)
SOs     = Portfolio_Small_Info$Stock_So
Tickers = Portfolio_Small_Info$Stock_Ticker

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks = Table_SEW_HR$Numb_Stocks[i]
  
  for (m in moneyness_levels) {
    result <- Hedge_Vanilla(
      sigma = SIGMAs[i], moneyness = m,
      N_shares = Number_of_Stocks,
      S = SOs[i], r = rate,
      lambda = lambda, muJ = muJ, sigmaJ = sigmaJ
    )
    
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  
  stock_hedges$Ticker = Tickers[i]
  Hedge_Vanilla_S_HR_EW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_S_HR_EW = do.call(rbind, Hedge_Vanilla_S_HR_EW)
rownames(Hedge_Vanilla_S_HR_EW) <- NULL
Hedge_Vanilla_S_HR_EW
View(Hedge_Vanilla_S_HR_EW)


#####Test####
library(derivmkts)
s=Portfolio_Small_Info$Stock_So[1]
k=0.9*s
v=SIGMAs[1]
r=High_rf 
tt= 63/252
d = 0              
lambda = 1
alphaJ= quantile(SnP600small_HR, 0.01)
sigmaJ =  sd(SnP600small_HR) 
mertonjump(s,k,v,r,tt,d,lambda,muJ,sigmaJ)
Merton_put_price (s, k, tt, r, v, lambda, alphaJ, sigmaJ, N_max = 40) 
#Give rather yield rather simialr results

########
sum_otm10 = sum(Hedge_Vanilla_S_HR_EW$Total_Cost[Hedge_Vanilla_S_HR_EW$Moneyness == "OTM_10"])
sum_otm10

sum_otm5 = sum(Hedge_Vanilla_S_HR_EW$Total_Cost[Hedge_Vanilla_S_HR_EW$Moneyness == "OTM_5"])
sum_otm5

sum_atm = sum(Hedge_Vanilla_S_HR_EW$Total_Cost[Hedge_Vanilla_S_HR_EW$Moneyness == "ATM_0"])
sum_atm

sum_itm5 = sum(Hedge_Vanilla_S_HR_EW$Total_Cost[Hedge_Vanilla_S_HR_EW$Moneyness == "ITM_5"])
sum_itm5




#################################### VW #####################################

Hedge_Vanilla_S_HR_VW = list()

moneyness_levels = c(0.9, 0.95, 1, 1.05)
names(moneyness_levels) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")


SIGMAs= Portfolio_Small_Info$Stock_Sigma_HR
SIGMAs=SIGMAs* sqrt(252) ## annaulize them 
SOs= Portfolio_Small_Info$Stock_So 
Tickers= Portfolio_Small_Info$Stock_Ticker

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks =  Table_SVW_HR$Numb_Stocks[i]
  
  for (m in moneyness_levels) {
    result <- Hedge_Vanilla(sigma = SIGMAs[i], moneyness = m, N_shares= Number_of_Stocks, S = SOs[i],r = rate,lambda=lambda, muJ=muJ, sigmaJ=sigmaJ)
    
    # Add a label for the moneyness
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  
  stock_hedges$Ticker = Tickers[i]
  
  Hedge_Vanilla_S_HR_VW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_S_HR_VW = do.call(rbind,Hedge_Vanilla_S_HR_VW)

# Reset row names
rownames(Hedge_Vanilla_S_HR_VW) = NULL

# Show result
Hedge_Vanilla_S_HR_VW
View(Hedge_Vanilla_S_HR_VW)

########
sum_otm10 = sum(Hedge_Vanilla_S_HR_VW$Total_Cost[Hedge_Vanilla_S_HR_VW$Moneyness == "OTM_10"])
sum_otm10

sum_otm5 =sum(Hedge_Vanilla_S_HR_VW$Total_Cost[Hedge_Vanilla_S_HR_VW$Moneyness == "OTM_5"])
sum_otm5

sum_atm= sum(Hedge_Vanilla_S_HR_VW$Total_Cost[Hedge_Vanilla_S_HR_VW$Moneyness == "ATM_0"])
sum_atm

sum_itm5 = sum(Hedge_Vanilla_S_HR_VW$Total_Cost[Hedge_Vanilla_S_HR_VW$Moneyness == "ITM_5"])
sum_itm5


#################################### IW #####################################

Hedge_Vanilla_S_HR_IW = list()

moneyness_levels = c(0.9, 0.95, 1, 1.05)
names(moneyness_levels) = c("OTM_10", "OTM_5", "ATM_0", "ITM_5")


SIGMAs= Portfolio_Small_Info$Stock_Sigma_HR
SIGMAs=SIGMAs* sqrt(252) ## annaulize them 
SOs= Portfolio_Small_Info$Stock_So 
Tickers= Portfolio_Small_Info$Stock_Ticker

for (i in 1:length(Tickers)) {
  stock_hedges = data.frame()
  Number_of_Stocks =  Table_SIW_HR$Numb_Stocks[i]
  
  for (m in moneyness_levels) {
    result = Hedge_Vanilla(sigma = SIGMAs[i], moneyness = m, N_shares= Number_of_Stocks, S = SOs[i],r = rate,lambda=lambda, muJ=muJ, sigmaJ=sigmaJ)
    
    # Add a label for the moneyness
    result$Moneyness = names(moneyness_levels[moneyness_levels == m])
    stock_hedges = rbind(stock_hedges, result)
  }
  
  stock_hedges$Ticker = Tickers[i]
  
  Hedge_Vanilla_S_HR_IW[[Tickers[i]]] = stock_hedges
}

Hedge_Vanilla_S_HR_IW = do.call(rbind,Hedge_Vanilla_S_HR_IW)

# Reset row names
rownames(Hedge_Vanilla_S_HR_IW) = NULL

# Show result
Hedge_Vanilla_S_HR_IW
View(Hedge_Vanilla_S_HR_IW)



########
sum_otm10 = sum(Hedge_Vanilla_S_HR_IW$Total_Cost[Hedge_Vanilla_S_HR_IW$Moneyness == "OTM_10"])
sum_otm10

sum_otm5 = sum(Hedge_Vanilla_S_HR_IW$Total_Cost[Hedge_Vanilla_S_HR_IW$Moneyness == "OTM_5"])
sum_otm5

sum_atm = sum(Hedge_Vanilla_S_HR_IW$Total_Cost[Hedge_Vanilla_S_HR_IW$Moneyness == "ATM_0"])
sum_atm

sum_itm5 = sum(Hedge_Vanilla_S_HR_IW$Total_Cost[Hedge_Vanilla_S_HR_IW$Moneyness == "ITM_5"])
sum_itm5

