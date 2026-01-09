############# Large Cap ####################
## Small Cap Betas - LR 
#BetaS

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

#####
Stocks_Returns_L_LR = list(NVDA_LR,MSFT_LR,AAPL_LR,AMZN_LR,META_LR,AVGO_LR,GOOGLa_LR,TSLA_LR,GOOGLc_LR,BRK_LR,ORCL_LR,JPM_LR)

Stocks_Returns_L_HR = list(NVDA_HR,MSFT_HR,AAPL_HR,AMZN_HR,META_HR,AVGO_HR,GOOGLa_HR,TSLA_HR,GOOGLc_HR,BRK_HR,ORCL_HR,JPM_HR)



compute_beta <- function(stock, etf) {
  cov_rm = cov(stock, etf)
  var_rm = var(etf)
  beta = cov_rm / var_rm
  return(beta)
}

## Large Cap Betas - LR
betas_L_LR = numeric(length(Stocks_Returns_L_LR))
for (i in seq_along(Stocks_Returns_L_LR)) {
  betas_L_LR[i] = compute_beta(Stocks_Returns_L_LR[[i]], SnP_500_LR)
}
betas_L_LR

## Large Cap Betas - HR
betas_L_HR <- numeric(length(Stocks_Returns_L_HR))

# Loop through each stock
for (i in seq_along(Stocks_Returns_L_HR)) {
  betas_L_HR[i] <- compute_beta(Stocks_Returns_L_HR[[i]], SnP_500_HR)
}

betas_L_HR





############### SMALL CAP ################
## Small Cap Betas - LR 
#BetaS

SnP600small_LR 
SnP600small_HR 

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
#####
Stocks_Returns_S_LR = list(AAR_LR,SXI_LR,BRKL_LR, SUPN_LR,BLFS_LR, ADEIA_LR, IAC_LR, CBKM_LR, BANF_LR, ROCK_LR, TR_LR, VIR_LR)
Stocks_Returns_S_HR = list(AAR_HR,SXI_HR,BRKL_HR, SUPN_HR,BLFS_HR, ADEIA_HR, IAC_HR, CBKM_HR, BANF_HR, ROCK_HR, TR_HR, VIR_HR)


compute_beta <- function(stock, etf) {
  cov_rm = cov(stock, etf)
  var_rm = var(etf)
  beta = cov_rm / var_rm
  return(beta)
}

## Small Cap Betas - LR
betas_LR = numeric(length(Stocks_Returns_S_LR))
for (i in seq_along(Stocks_Returns_S_LR)) {
  betas_LR[i] = compute_beta(Stocks_Returns_S_LR[[i]], SnP600small_LR)
}
betas_LR

mean_betas_LR = mean(betas_LR)
mean_betas_LR

## Small Cap Betas - HR
betas_HR <- numeric(length(Stocks_Returns_S_HR))

# Loop through each stock
for (i in seq_along(Stocks_Returns_S_LR)) {
  betas_HR[i] <- compute_beta(Stocks_Returns_S_HR[[i]], SnP600small_HR)
}

betas_HR

mean_betas_HR = mean(betas_HR)
mean_betas_HR




################## HISTORICAL BETAS ######################
Beta_L_LR_EW = sum(betas_L_LR*(Table_LEW_LR$Numb_Stocks/sum(Table_LEW_LR$Numb_Stock)))
Beta_L_LR_VW = sum(betas_L_LR*(Table_LVW_LR$Numb_Stocks/sum(Table_LVW_LR$Numb_Stock)))
Beta_L_LR_IW =sum(betas_L_LR*(Table_LIW_LR$Numb_Stocks/sum(Table_LIW_LR$Numb_Stock)))

Beta_L_HR_EW =sum(betas_L_HR*(Table_LEW_HR$Numb_Stocks/sum(Table_LEW_HR$Numb_Stock)))
Beta_L_HR_VW = sum(betas_L_HR*(Table_LVW_HR$Numb_Stocks/sum(Table_LVW_HR$Numb_Stock)))
Beta_L_HR_IW = sum(betas_L_HR*(Table_LIW_HR$Numb_Stocks/sum(Table_LIW_HR$Numb_Stock)))

Beta_S_LR_EW = sum(betas_LR*(Table_SEW_LR$Numb_Stocks/sum(Table_SEW_LR$Numb_Stock)))
Beta_S_LR_VW = sum(betas_LR*(Table_SVW_LR$Numb_Stocks/sum(Table_SVW_LR$Numb_Stock)))
Beta_S_LR_IW =sum(betas_LR*(Table_SIW_LR$Numb_Stocks/sum(Table_SIW_LR$Numb_Stock)))

Beta_S_HR_EW = sum(betas_HR*(Table_SEW_HR$Numb_Stocks/sum(Table_SEW_HR$Numb_Stock)))
Beta_S_HR_VW = sum(betas_HR*(Table_SVW_HR$Numb_Stocks/sum(Table_SVW_HR$Numb_Stock)))
Beta_S_HR_IW = sum(betas_HR*(Table_SIW_HR$Numb_Stocks/sum(Table_SIW_HR$Numb_Stock)))


Beta_Table=data.frame(Weights= c('EW', 'VW', 'IW'),
                      Large_Low_Rate = c(Beta_L_LR_EW,Beta_L_LR_VW,Beta_L_LR_IW),
                      Large_High_Rate = c(Beta_L_HR_EW,Beta_L_HR_VW,Beta_L_HR_IW),
                      Small_Low_Rate =  c(Beta_S_LR_EW,Beta_S_LR_VW,Beta_S_LR_IW),
                      Small_High_Rate = c(Beta_S_HR_EW,Beta_S_HR_VW,Beta_S_HR_IW))
