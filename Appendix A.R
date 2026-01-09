################# Appendix A ##################
#This appendix includes all graphs referred to in the text, alongside all in-text calculations and numerical precisions.

################ Tables ###################

####### Table 1 – Stock Information Table for Large Cap Equities in Annualized Terms (p.19) #######
#C.f. Appendix B, "II - Portfolio Info.R"

# Number of trading days in a year
N <- 252

# Portfolio Large with arithmetic annualized values
Portfolio_Large_Info_Annualized <- data.frame(
  Stock_Names = c('Nvidia Corp.', 'Microsoft Corp', 'Apple Inc.','Amazon.com Inc','Meta Platforms Inc.', 
                  'Broadcom Inc.', 'Alphabet Inc. (class A) ','Tesla Inc.','Alphabet Inc. (class C)',
                  'Berkshire Hathaway','Oracle Corporation', 'Jpmorgan Chase & Co'),
  
  Stock_Ticker = c('NVDA', 'MSFT', 'AAPL','AMZN', 'META', 'AVGO', 'GOOGL A', 'TSLA', 'GOOGL C','BRK', 
                   'ORCL','JPM'),
  
  Stock_So = c(So_NVDA, So_MSFT, So_AAPL, So_AMZN, So_META, So_AVGO, So_GOOGLa, So_TSLA, So_GOOGLc, 
               So_BRK, So_ORCL, So_JPM),
  
  # Annualized returns (arithmetic)
  Stock_MU_HR_Annual = c(mu_NVDA_HR, mu_MSFT_HR, mu_AAPL_HR, mu_AMZN_HR, mu_META_HR, mu_AVGO_HR, 
                         mu_GOOGLa_HR, mu_TSLA_HR, mu_GOOGLc_HR, mu_BRK_HR, mu_ORCL_HR, mu_JPM_HR) * N,
  Stock_MU_LR_Annual = c(mu_NVDA_LR, mu_MFST_LR, mu_AAPL_LR, mu_AMZN_LR, mu_META_LR, mu_AVGO_LR, 
                         mu_GOOGLa_LR, mu_TSLA_LR, mu_GOOGLc_LR, mu_BRK_LR, mu_ORCL_LR, mu_JPM_LR) * N,
  
  # Annualized volatility
  Stock_SIGMA_HR_Annual = c(sigma_NVDA_HR, sigma_MSFT_HR, sigma_AAPL_HR, sigma_AMZN_HR, sigma_META_HR,
                            sigma_AVGO_HR, sigma_GOOGLa_HR, sigma_TSLA_HR, sigma_GOOGLc_HR, sigma_BRK_HR,
                            sigma_ORCL_HR, sigma_JPM_HR) * sqrt(N),
  Stock_SIGMA_LR_Annual = c(sigma_NVDA_LR, sigma_MSFT_LR, sigma_AAPL_LR, sigma_AMZN_LR, sigma_META_LR,
                            sigma_AVGO_LR, sigma_GOOGLa_LR, sigma_TSLA_LR, sigma_GOOGLc_LR, sigma_BRK_LR,
                            sigma_ORCL_LR, sigma_JPM_LR) * sqrt(N)
)

View(Portfolio_Large_Info_Annualized)



####### Table 2 – Stock Information Table for Small Cap Equities in Annualized Terms (p.19) #######
#from Part.2 - Portfolio Info

# Number of trading days in a year
N <- 252
# Portfolio Small with arithmetic annualized values
Portfolio_Small_Info_Annualized <- data.frame(
  Stock_Names = c('AAR Corp.', 'Standex International Corp', 'Brookline Bancorp', 'Supernus Pharm', 
                  'Biolife Solutions','Adeia Inc','Iac Inc', 'Customers Bancorp','Bancfirst Corp',
                  'Gibraltar Ind Inc','Tootsie Roll Industries','Vir Biotechnology Inc'),
  
  Stock_Ticker = c('AIR', 'SXI', 'BRKL', 'SUPN','BLFS','ADEIA', 'IAC','CBKM', 'BANF', 
                   'ROCK', 'TR', 'VIR'),
  
  Stock_So = c(So_AAR, So_SXI, So_BRKL, So_SUPN, So_BLFS, So_ADEIA, So_IAC, So_CBKM, 
               So_BANF, So_ROCK, So_TR, So_VIR),
  
  # Annualized returns (arithmetic)
  Stock_MU_HR_Annual = c(mu_AAR_HR, mu_SXI_HR, mu_BRKL_HR, mu_SUPN_HR, mu_BLFS_HR, mu_ADEIA_HR, 
                         mu_IAC_HR, mu_CBKM_HR, mu_BANF_HR, mu_ROCK_HR, mu_TR_HR, mu_VIR_HR) * N,
  Stock_MU_LR_Annual = c(mu_AAR_LR, mu_SXI_LR, mu_BRKL_LR, mu_SUPN_LR, mu_BLFS_LR, mu_ADEIA_LR, 
                         mu_IAC_LR, mu_CBKM_LR, mu_BANF_LR, mu_ROCK_LR, mu_TR_LR, mu_VIR_LR) * N,
  
  # Annualized volatility
  Stock_SIGMA_HR_Annual = c(sigma_AAR_HR, sigma_SXI_HR, sigma_BRKL_HR, sigma_SUPN_HR, sigma_BLFS_HR,
                            sigma_ADEIA_HR, sigma_IAC_HR, sigma_CBKM_HR, sigma_BANF_HR, sigma_ROCK_HR,
                            sigma_TR_HR, sigma_VIR_HR) * sqrt(N),
  Stock_SIGMA_LR_Annual = c(sigma_AAR_LR, sigma_SXI_LR, sigma_BRKL_LR, sigma_SUPN_LR, sigma_BLFS_LR,
                            sigma_ADEIA_LR, sigma_IAC_LR, sigma_CBKM_LR, sigma_BANF_LR, sigma_ROCK_LR,
                            sigma_TR_LR, sigma_VIR_LR) * sqrt(N)
)

View(Portfolio_Small_Info_Annualized)


####### Table 3 - Summary of Results Across All Conditions (p.29) #######

###final table: 
final_results_table = data.frame(
  Row = c("Hedge Effectiveness","Value","CVaR99","Value","Sharpe Ratio","Value","Mean Terminal Value","Value","Highest Unhedged","Value","Total Hedging Costs","Value"),
  `Large Cap - High Rates` = c("ITM5_IW_Vanilla",0.533,"ITM5_IW_Vanilla",9634848,"OTM10_IW_Vanilla",0.572,"OTM10_VW_Index",10935667,"VW",10987070,"ITM5_VW_Vanilla",1014274),
  `Large Cap - Low Rates` = c("ITM5_IW_Vanilla", 0.508,"ITM5_IW_Vanilla",9541135,"OTM10_IW_Vanilla",0.664,"OTM10_VW_Index",11024506,"VW",11083892,"ITM5_VW_Vanilla",1093905),
  `Small Cap - High Rates` = c("ITM5_IW_Vanilla",0.702,"ITM5_IW_Vanilla",9548805,"ITM5_IW_Vanilla",0.019,"ITM5_IW_Vanilla",10130361,"IW",10098093,"ITM5_VW_Vanilla",1125834),
  `Small Cap - Low Rates` = c("ITM5_IW_Vanilla",0.548,"ITM5_IW_Vanilla",9357513,"OTM10_IW_Vanilla",0.514,"OTM10_EW_Index",10591928,"EW",10660308,"ITM5_VW_Vanilla",1407868),
  stringsAsFactors = FALSE
)
View(final_results_table)



########### Figures ###########

####### Figure 1 - Long Put Position Payoff at Maturity (p.8) ####### 
library(ggplot2)
K <- 100; premium <- 5; n <- 100
S <- seq(70, 130, 1)  
PL <- (pmax(K - S, 0) - premium) * n

ggplot(data.frame(S, PL), aes(S, PL)) +
  geom_line(color = "blue", size = 2.5) +                  
  geom_vline(xintercept = K, linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 0, size = 1.5) +
  labs(x = "Stock Price at Maturity ($)", 
       y = "Profit / Loss ($)") +
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(
    axis.title = element_text(face = "bold", size = 22),     
    axis.text = element_text(face = "bold", size = 20),      # Bigger,
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.margin = margin(15, 15, 15, 15)
  ) 
  


####### Figure 2 - Long Put Position Payoff at Maturity (p.8) ####### 
library(ggplot2)

# Parameters
S0 <- 100           # Current stock price
K <- 100            # Strike price
premium <- 5        # Put premium per share
n <- 100            # Shares per option

# Stock prices at maturity
S <- seq(70, 130, 1)

# Profit/Loss calculations
stock_PL <- (S - S0) * n
put_PL <- (pmax(K - S, 0) - premium) * n
protective_PL <- stock_PL + put_PL

# Create data frame
df <- data.frame(
  StockPrice = S,
  Stock = stock_PL,
  Put = put_PL,
  Protective = protective_PL
)

# Plot
ggplot(df, aes(x = StockPrice)) +
  geom_line(aes(y = Stock, color = "Stock Only"), size = 2.5) +
  geom_line(aes(y = Put, color = "Long Put"), size = 2.5) +
  geom_line(aes(y = Protective, color = "Protective Put"), size = 2.5) +
  geom_vline(xintercept = K, linetype = "dashed", size = 1.5, color = "black") +
  geom_hline(yintercept = 0, size = 1.5) +
  labs(x = "Stock Price at Maturity ($)",
       y = "Profit / Loss ($)",
       color = "Position") +
  scale_color_manual(values = c("Stock Only" = "red", "Long Put" = "blue", "Protective Put" = "darkgreen")) +
  theme_minimal(base_size = 18, base_family = "Helvetica") +
  theme(
    axis.title = element_text(face = "bold", size = 22),
    axis.text = element_text(face = "bold", size = 20),
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(face = "bold", size = 18),
    plot.margin = margin(15, 15, 15, 15)
  )


####### Figure 3 - Time Series of the U.S. 3-Month Treasury Yield (p.18) ####### 

# See "I - Loading Data.R" in Appendix B 

####### Figures 4 - Density Distribution of the U.S. 3-Month Treasury Yield (p.18) ####### 

# See "I - Loading Data.R" in Appendix B 

####### Figure 5 - Interest Rate Regimes in the U.S. 3-Month Treasury Yield (p.19) ####### 

# See "I - Loading Data.R" in Appendix B 


########## Kappa Calculation (p.15) #############

muJ_large_high  = quantile(SnP_500_HR, 0.01)  
muJ_large_low    = quantile(SnP_500_LR, 0.01)  
muJ_small_high = quantile(SnP600small_HR, 0.01) 
muJ_small_low = quantile(SnP600small_LR, 0.01) 

sigmaJ_large_high  = sd(SnP_500_HR)
sigmaJ_large_low  = sd(SnP_500_LR)
sigmaJ_small_high = sd(SnP600small_HR)
sigmaJ_small_low  = sd(SnP600small_LR)


kappa_large_high=exp(muJ_large_high + 0.5 * sigmaJ_large_high^2) - 1
kappa_large_high*100

kappa_large_low  =exp(muJ_large_low  + 0.5 * sigmaJ_large_low^2)  - 1
kappa_large_low*100

kappa_small_high= exp(muJ_small_high + 0.5 * sigmaJ_small_high^2) - 1
kappa_small_high*100

kappa_small_low  = exp(muJ_small_low  + 0.5 * sigmaJ_small_low^2)  - 1
kappa_small_low*100



######### proba N>20 Calculation (p.19)#########
lambda = 1
k = 20
prob <- 1 - ppois(k, lambda)
prob   


##### Return and Volalitiy spread for the two rates (p.25)######

Spread_MU_L = Portfolio_Large_Info_Annualized$Stock_MU_HR_Annual - Portfolio_Large_Info_Annualized$Stock_MU_LR_Annual
mean(Spread_MU_L)

Spread_MU_S = Portfolio_Small_Info_Annualized$Stock_MU_HR_Annual - Portfolio_Small_Info_Annualized$Stock_MU_LR_Annual
mean(Spread_MU_S)

Spread_Sigma_L = Portfolio_Large_Info_Annualized$Stock_SIGMA_HR_Annual - Portfolio_Large_Info_Annualized$Stock_SIGMA_LR_Annual
mean(Spread_Sigma_L)

Spread_Sigma_S = Portfolio_Small_Info_Annualized$Stock_SIGMA_HR_Annual - Portfolio_Small_Info_Annualized$Stock_SIGMA_LR_Annual
mean(Spread_Sigma_S)


####### Correlation Comparison -- Historical Data vs. Simulations (p.29)

# From "IV - Correlation Matrixes.R" in Appendix B and "3 - Simulation.R" in the sub-folders 

w = rep(1/12, 12)

# Compute EW portfolio returns
EW_Portfolio_S_HR <- as.numeric(stocks_S_HR %*% w)
EW_Portfolio_S_LR <- as.numeric(stocks_S_LR %*% w)
EW_Portfolio_L_HR <- as.numeric(stocks_L_HR %*% w)
EW_Portfolio_L_LR <- as.numeric(stocks_L_LR %*% w)

# Compute correlation directly with index (both are vectors)
pathwise_corr_S_HR <- cor(EW_Portfolio_S_HR, index_S_HR, use = "complete.obs")
pathwise_corr_S_LR <- cor(EW_Portfolio_S_LR, index_S_LR, use = "complete.obs")
pathwise_corr_L_HR <- cor(EW_Portfolio_L_HR, index_L_HR, use = "complete.obs")
pathwise_corr_L_LR <- cor(EW_Portfolio_L_LR, index_L_LR, use = "complete.obs")

EW_Index_Correlations <- data.frame(
  Market = c("Small Caps", "Small Caps", "Large Caps", "Large Caps"),
  Regime = c("High Rate", "Low Rate", "High Rate", "Low Rate"),
  EW_Index_Correlation = c(pathwise_corr_S_HR,
                           pathwise_corr_S_LR,
                           pathwise_corr_L_HR,
                           pathwise_corr_L_LR)
)

EW_Index_Correlations

mean(EW_Index_Correlations$EW_Index_Correlation)

Correlation_Results_Table = data.frame(
  Market = c("Large", "Large", "Small", "Small"),
  Regime = c("High Rate", "Low Rate", "High Rate", "Low Rate"),
  `Historic` = c(0.9215, 0.9183, 0.8927, 0.8715),
  `Simulation`   = c(0.3148, 0.3044, 0.3449, 0.3568),
  stringsAsFactors = FALSE
)

View(Correlation_Results_Table)

mean(c(0.3148, 0.3044, 0.3449, 0.3568))
mean(c(0.9215, 0.9183, 0.8927, 0.8715))



