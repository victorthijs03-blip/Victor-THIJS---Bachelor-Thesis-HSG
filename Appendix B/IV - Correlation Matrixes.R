###################### LARGE CAPS #########################

#### S&P 500 ####
library(quantmod)
getSymbols("^GSPC", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(GSPC)
SnP_500 = diff(log(closing_prices))  #diff(log(closing_prices)) automatically computes log(Pt/Pt−1) for all consecutive days.
SnP_500 <- na.omit(SnP_500)
SnP_500_LR = (SnP_500$GSPC.Close[1:604])
SnP_500_HR = (SnP_500$GSPC.Close[605:1340])




#### Large Stocks ####
NVDA_LR = NVDA_raw$NVDA.Close[1:604]
NVDA_HR = NVDA_raw$NVDA.Close[605:1340]

MSFT_LR = MSFT_raw$MSFT.Close[1:604]
MSFT_HR = MSFT_raw$MSFT.Close[605:1340]

AAPL_LR = AAPL_raw$AAPL.Close[1:604]
AAPL_HR = AAPL_raw$AAPL.Close[605:1340]

AMZN_LR = AMZN_raw$AMZN.Close[1:604]
AMZN_HR = AMZN_raw$AMZN.Close[605:1340]

META_LR = META_raw$META.Close[1:604]
META_HR = META_raw$META.Close[605:1340]

AVGO_LR = AVGO_raw$AVGO.Close[1:604]
AVGO_HR = AVGO_raw$AVGO.Close[605:1340]

GOOGLa_LR = GOOGLa_raw$GOOGL.Close[1:604]
GOOGLa_HR = GOOGLa_raw$GOOGL.Close[605:1340]

TSLA_LR = TSLA_raw$TSLA.Close[1:604]
TSLA_HR = TSLA_raw$TSLA.Close[605:1340]

GOOGLc_LR = GOOGLc_raw$GOOG.Close[1:604]
GOOGLc_HR= GOOGLc_raw$GOOG.Close[605:1340]

BRK_LR = BRK_raw$`BRK-B.Close`[1:604]
BRK_HR = BRK_raw$`BRK-B.Close`[605:1340]

ORCL_LR = ORCL_raw$ORCL.Close[1:604]
ORCL_HR = ORCL_raw$ORCL.Close[605:1340]

JPM_LR = JPM_raw$JPM.Close[1:604]
JPM_HR = JPM_raw$JPM.Close[605:1340]



### LOW INTEREST RATE REGIME #############################################
index_L_LR= SnP_500_LR
colnames(index_L_LR) = "SnP500"
index_L_LR  #daily log returns

stocks_L_LR = cbind(NVDA_LR,MSFT_LR,AAPL_LR,AMZN_LR,META_LR,AVGO_LR,GOOGLa_LR,TSLA_LR,GOOGLc_LR,BRK_LR,ORCL_LR,JPM_LR)
colnames(stocks_L_LR) = c("NVDA","MSFT","AAPL","AMZN","META","AVGO","GOOGLa","TSLA","GOOGLc","BRK","ORCL","JPM")
stocks_L_LR #daily log returns


Combined_Returns_L_LR = cbind(stocks_L_LR, index_L_LR)

#covariance (daily)
cov_matrix_L_LR = cov(Combined_Returns_L_LR, use = "complete.obs")
cov_matrix_L_LR
View(cov_matrix_L_LR)

#correlation (daily)
corr_matrix_L_LR = cor(Combined_Returns_L_LR, use = "complete.obs")
corr_matrix_L_LR

View(corr_matrix_L_LR)


### HIGH INTEREST RATE REGIME #############################################
index_L_HR= SnP_500_HR
colnames(index_L_HR) = "SnP500"
index_L_HR  #daily log returns

stocks_L_HR = cbind(NVDA_HR,MSFT_HR,AAPL_HR,AMZN_HR,META_HR,AVGO_HR,GOOGLa_HR,TSLA_HR,GOOGLc_HR,BRK_HR,ORCL_HR,JPM_HR)
colnames(stocks_L_HR) = c("NVDA","MSFT","AAPL","AMZN","META","AVGO","GOOGLa","TSLA","GOOGLc","BRK","ORCL","JPM")
stocks_L_HR #daily log returns

Combined_Returns_L_HR = cbind(stocks_L_HR, index_L_HR)

#covariance (daily)
cov_matrix_L_HR = cov(Combined_Returns_L_HR, use = "complete.obs")
cov_matrix_L_HR

#correlation (daily)
corr_matrix_L_HR = cor(Combined_Returns_L_HR)
corr_matrix_L_HR



###################### SMALL CAPS ##########################

#### S&P SmallCap 600
library(quantmod)
getSymbols("^SP600", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(SP600)
SnP600small<- diff(log(closing_prices))  #diff(log(closing_prices)) automatically computes log(Pt/Pt−1) for all consecutive days.
SnP600small <- na.omit(SnP600small)
SnP600small_LR = (SnP600small$SP600.Close[1:604])
SnP600small_HR = (SnP600small$SP600.Close[605:1340])



#### Small Stocks ####
AAR_LR = AAR_raw$AIR.Close[1:604]
AAR_HR = AAR_raw$AIR.Close[605:1340]

SXI_LR = SXI_raw$SXI.Close[1:604]
SXI_HR = SXI_raw$SXI.Close[605:1340]

BRKL_LR = BRKL_raw$BBT.Close[1:604]
BRKL_HR = BRKL_raw$BBT.Close[605:1340]

SUPN_LR = SUPN_raw$SUPN.Close[1:604]
SUPN_HR = SUPN_raw$SUPN.Close[605:1340]

BLFS_LR = BLFS_raw$BLFS.Close[1:604]
BLFS_HR = BLFS_raw$BLFS.Close[605:1340]

ADEIA_LR = ADEIA_raw$ADEA.Close[1:604]
ADEIA_HR = ADEIA_raw$ADEA.Close[605:1340]

IAC_LR = IAC_raw$IAC.Close[1:604]
IAC_HR = IAC_raw$IAC.Close[605:1340]

CBKM_LR = CBKM_raw$CBKM.Close[1:604]
CBKM_HR = CBKM_raw$CBKM.Close[605:1340]

BANF_LR = BANF_raw$BANF.Close[1:604]
BANF_HR = BANF_raw$BANF.Close[605:1340]

ROCK_LR = ROCK_raw$ROCK.Close[1:604]
ROCK_HR = ROCK_raw$ROCK.Close[605:1340]

TR_LR = TR_raw$TR.Close[1:604]
TR_HR = TR_raw$TR.Close[605:1340]

VIR_LR = VIR_raw$VIR.Close[1:604]
VIR_HR = VIR_raw$VIR.Close[605:1340]



### LOW INTEREST RATE REGIME #############################################
index_S_LR= SnP600small_LR
colnames(index_S_LR) = "SnP600"
index_S_LR  #daily log returns

stocks_S_LR = cbind(AAR_LR,SXI_LR,BRKL_LR,SUPN_LR,BLFS_LR,ADEIA_LR,IAC_LR,CBKM_LR,BANF_LR,ROCK_LR,TR_LR,VIR_LR)
colnames(stocks_S_LR) = c("AAR","SXI","BRKL","SUPN","BLFS","ADEIA","IAC","CBKM","BANF","ROCK","TR","VIR")
stocks_S_LR #daily log returns

Combined_Returns_S_LR = cbind(stocks_S_LR, index_S_LR)

#covariance (daily)
cov_matrix_S_LR = cov(Combined_Returns_S_LR, use = "complete.obs")
cov_matrix_S_LR

#correlation (daily)
corr_matrix_S_LR = cor(Combined_Returns_S_LR, use = "complete.obs")
corr_matrix_S_LR



### HIGH INTEREST RATE REGIME #############################################
index_S_HR= SnP600small_HR
colnames(index_S_HR) = "SnP600"
index_S_HR  #daily log returns

stocks_S_HR = cbind(AAR_HR,SXI_HR,BRKL_HR,SUPN_HR,BLFS_HR,ADEIA_HR,IAC_HR,CBKM_HR,BANF_HR,ROCK_HR,TR_HR,VIR_HR)
colnames(stocks_S_HR) = c("AAR","SXI","BRKL","SUPN","BLFS","ADEIA","IAC","CBKM","BANF","ROCK","TR","VIR")
stocks_S_HR #daily log returns

Combined_Returns_S_HR = cbind(stocks_S_HR, index_S_HR)

#covariance (daily)
cov_matrix_S_HR = cov(Combined_Returns_S_HR, use = "complete.obs")
cov_matrix_S_HR

#correlation (daily)
corr_matrix_S_HR = cor(Combined_Returns_S_HR)
corr_matrix_S_HR


######## For Discussion Purposes ##########

## Historical Corr vs. simulation Corr
## HISTORICAL MEAN CORRELATION – EW PORTFOLIOS vs INDEX


w <- rep(1/12, 12)

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
