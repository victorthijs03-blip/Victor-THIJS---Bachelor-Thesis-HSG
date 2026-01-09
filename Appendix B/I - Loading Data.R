####### Part. 1 -- Loading Data ######

library(dplyr)
library(quantmod)

## daily returns from 1 Apr 2020 to 1 Aug 2025

##### Small valuation portfolio (choose twelve randomly from us small cap index) ####
# choose 12 random stocks from the S&P600 small cap index as of 04/04 (from https://www.barchart.com/stocks/indices/sp/sp600?viewName=main )

List_SnP600 = read.csv("sp-600-index-04-04-2025.csv")
set.seed(777)
randomly_choosen12 = sample(List_SnP600$Name, 12)
randomly_choosen12
#=> AAR Corp, Standex International Corp, Brookline Bancorp, Supernus Pharm
#=> Biolife Solutions, Adeia Inc, Iac Inc, Customers Bancorp
#=> Bancfirst Corp, Gibraltar Ind Inc, Tootsie Roll Industries, Vir Biotechnology Inc

library(quantmod)
# ... AAR Corp. -- AIR ...
getSymbols("AIR", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(AIR)
AAR_raw <- diff(log(closing_prices))  #diff(log(closing_prices)) automatically computes log(Pt/Pt−1) for all consecutive days.
AAR_raw <- na.omit(AAR_raw)

mu_AAR_LR = mean(AAR_raw$AIR.Close[1:604])
mu_AAR_HR = mean(AAR_raw$AIR.Close[605:1340])

mu_AAR_LR #0.001696328 [ie. 0.1696328%] --> Mean daily returns under Low Interest Regime 
mu_AAR_HR #0.0006290574 [ie. 0.06290574%] --> Mean daily returns under High Interest Regime 

#volatility = standard deviation of the returns
sigma_AAR_LR = sd(AAR_raw$AIR.Close[1:604])
sigma_AAR_HR = sd(AAR_raw$AIR.Close[605:1340])

sigma_AAR_HR
#2,162062%
sigma_AAR_LR
#3.354884%

So_AAR = as.numeric(closing_prices$AIR.Close[1341,])
#72.99

# ... Standex International Corp -- SXI ...
getSymbols("SXI", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(SXI)
SXI_raw <- diff(log(closing_prices))
SXI_raw <- na.omit(SXI_raw)

mu_SXI_LR = mean(SXI_raw$SXI.Close[1:604])
mu_SXI_HR = mean(SXI_raw$SXI.Close[605:1340])

#volatility = standard deviation of the returns
sigma_SXI_LR = sd(SXI_raw$SXI.Close[1:604])
sigma_SXI_HR = sd(SXI_raw$SXI.Close[605:1340])

So_SXI = as.numeric(closing_prices$SXI.Close[1341,])

# ... Brookline Bancorp -- BRKL ... ####==> merged on Sep. 1 with Beacon Bank & Trust to form Beacon Financial Corporation.
getSymbols("BBT", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(BBT)
BRKL_raw <- diff(log(closing_prices))
BRKL_raw <- na.omit(BRKL_raw)

mu_BRKL_LR = mean(BRKL_raw$BBT.Close[1:604])
mu_BRKL_HR = mean(BRKL_raw$BBT.Close[605:1340])

#volatility = standard deviation of the returns
sigma_BRKL_LR = sd(BRKL_raw$BBT.Close[1:604])
sigma_BRKL_HR = sd(BRKL_raw$BBT.Close[605:1340])

So_BRKL = as.numeric(closing_prices$BBT.Close[1341,])
So_BRKL

# ... Supernus Pharm -- SUPN ...
getSymbols("SUPN", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(SUPN)
SUPN_raw <- diff(log(closing_prices))
SUPN_raw <- na.omit(SUPN_raw)

mu_SUPN_LR = mean(SUPN_raw$SUPN.Close[1:604])
mu_SUPN_HR = mean(SUPN_raw$SUPN.Close[605:1340])

#volatility = standard deviation of the returns
sigma_SUPN_LR = sd(SUPN_raw$SUPN.Close[1:604])
sigma_SUPN_HR = sd(SUPN_raw$SUPN.Close[605:1340])

So_SUPN = as.numeric(closing_prices$SUPN.Close[1341,])
So_SUPN



# ... Biolife Solutions -- BLFS ...
getSymbols("BLFS", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(BLFS)
BLFS_raw <- diff(log(closing_prices))
BLFS_raw <- na.omit(BLFS_raw)

mu_BLFS_LR = mean(BLFS_raw$BLFS.Close[1:604])
mu_BLFS_HR = mean(BLFS_raw$BLFS.Close[605:1340])

#volatility = standard deviation of the returns
sigma_BLFS_LR = sd(BLFS_raw$BLFS.Close[1:604])
sigma_BLFS_HR = sd(BLFS_raw$BLFS.Close[605:1340])

So_BLFS = as.numeric(closing_prices$BLFS.Close[1341,])
So_BLFS

# ... Adeia Inc -- ADEA ...
getSymbols("ADEA", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(ADEA)
ADEIA_raw <- diff(log(closing_prices))
ADEIA_raw <- na.omit(ADEIA_raw)

mu_ADEIA_LR = mean(ADEIA_raw$ADEA.Close[1:604])
mu_ADEIA_HR = mean(ADEIA_raw$ADEA.Close[605:1340])

#volatility = standard deviation of the returns
sigma_ADEIA_LR = sd(ADEIA_raw$ADEA.Close[1:604])
sigma_ADEIA_HR = sd(ADEIA_raw$ADEA.Close[605:1340])

So_ADEIA = as.numeric(closing_prices$ADEA.Close[1341,])
So_ADEIA

# ... Iac Inc -- IAC ...
getSymbols("IAC", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(IAC)
IAC_raw <- diff(log(closing_prices))
IAC_raw <- na.omit(IAC_raw)

mu_IAC_LR = mean(IAC_raw$IAC.Close[1:604])
mu_IAC_HR = mean(IAC_raw$IAC.Close[605:1340])

#volatility = standard deviation of the returns
sigma_IAC_LR = sd(IAC_raw$IAC.Close[1:604])
sigma_IAC_HR = sd(IAC_raw$IAC.Close[605:1340])

So_IAC = as.numeric(closing_prices$IAC.Close[1341,])
So_IAC

# ... Customers Bancorp -- CBKM ...
getSymbols("CBKM", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(CBKM)
CBKM_raw <- diff(log(closing_prices))
CBKM_raw <- na.omit(CBKM_raw)

mu_CBKM_LR = mean(CBKM_raw$CBKM.Close[1:604])
mu_CBKM_HR = mean(CBKM_raw$CBKM.Close[605:1340])

#volatility = standard deviation of the returns
sigma_CBKM_LR = sd(CBKM_raw$CBKM.Close[1:604])
sigma_CBKM_HR = sd(CBKM_raw$CBKM.Close[605:1340])

So_CBKM = as.numeric(closing_prices$CBKM.Close[1341,])
So_CBKM

# ... Bancfirst Corp -- BANF ...
getSymbols("BANF", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(BANF)
BANF_raw <- diff(log(closing_prices))
BANF_raw <- na.omit(BANF_raw)

mu_BANF_LR = mean(BANF_raw$BANF.Close[1:604])
mu_BANF_HR = mean(BANF_raw$BANF.Close[605:1340])

#volatility = standard deviation of the returns
sigma_BANF_LR = sd(BANF_raw$BANF.Close[1:604])
sigma_BANF_HR = sd(BANF_raw$BANF.Close[605:1340])

So_BANF = as.numeric(closing_prices$BANF.Close[1341,])
So_BANF

# ... Gibraltar Ind Inc -- ROCK ...
getSymbols("ROCK", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(ROCK)
ROCK_raw <- diff(log(closing_prices))
ROCK_raw <- na.omit(ROCK_raw)

mu_ROCK_LR = mean(ROCK_raw$ROCK.Close[1:604])
mu_ROCK_HR = mean(ROCK_raw$ROCK.Close[605:1340])

#volatility = standard deviation of the returns
sigma_ROCK_LR = sd(ROCK_raw$ROCK.Close[1:604])
sigma_ROCK_HR = sd(ROCK_raw$ROCK.Close[605:1340])

So_ROCK = as.numeric(closing_prices$ROCK.Close[1341,])
So_ROCK

# ... Tootsie Roll Industries -- TR ...
getSymbols("TR", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(TR)
TR_raw <- diff(log(closing_prices))
TR_raw <- na.omit(TR_raw)

mu_TR_LR = mean(TR_raw$TR.Close[1:604])
mu_TR_HR = mean(TR_raw$TR.Close[605:1340])

#volatility = standard deviation of the returns
sigma_TR_LR = sd(TR_raw$TR.Close[1:604])
sigma_TR_HR = sd(TR_raw$TR.Close[605:1340])

So_TR = as.numeric(closing_prices$TR.Close[1341,])
So_TR

# ... Vir Biotechnology Inc -- VIR
getSymbols("VIR", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(VIR)
closing_prices

VIR_raw <- diff(log(closing_prices))
VIR_raw <- na.omit(VIR_raw)

mu_VIR_LR = mean(VIR_raw$VIR.Close[1:604])
mu_VIR_HR = mean(VIR_raw$VIR.Close[605:1340])

#volatility = standard deviation of the returns
sigma_VIR_LR = sd(VIR_raw$VIR.Close[1:604])
sigma_VIR_HR = sd(VIR_raw$VIR.Close[605:1340])

So_VIR = as.numeric(closing_prices$VIR.Close[1341,])
So_VIR

#### Large valuation portfolio (twelve biggest of S&P) #### as of April 1st. https://www.slickcharts.com/sp500
#--------------------------------------------------------------------

#=> Nvidia, Microsoft, Apple. inc, Amazon, Meta, Broadcom, Alphabet Class A., Tesla, Alphabet ClassB, 
#=> Berkshire, Orcale, JPMorgan 

library(quantmod)
# I ... Nvdia Corp. -- NVDA ...
getSymbols("NVDA", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(NVDA)
NVDA_raw <- diff(log(closing_prices))  #diff(log(closing_prices)) automatically computes log(Pt/Pt−1) for all consecutive days.
NVDA_raw <- na.omit(NVDA_raw)

mu_NVDA_LR = mean(NVDA_raw$NVDA.Close[1:604])
mu_NVDA_HR = mean(NVDA_raw$NVDA.Close[605:1340])

sigma_NVDA_LR = sd(NVDA_raw$NVDA.Close[1:604])
sigma_NVDA_HR = sd(NVDA_raw$NVDA.Close[605:1340])

So_NVDA = as.numeric(closing_prices$NVDA.Close[1341,])

# II... Microsoft -- MSFT ...
getSymbols("MSFT", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(MSFT)
MSFT_raw <- diff(log(closing_prices))
MSFT_raw <- na.omit(MSFT_raw)
mu_MFST_LR = mean(MSFT_raw$MSFT.Close[1:604])
mu_MSFT_HR = mean(MSFT_raw$MSFT.Close[605:1340])
#volatility = standard deviation of the returns
sigma_MSFT_LR = sd(MSFT_raw$MSFT.Close[1:604])
sigma_MSFT_HR = sd(MSFT_raw$MSFT.Close[605:1340])
So_MSFT = as.numeric(closing_prices$MSFT.Close[1341,])

# III ... Apple Inc -- AAPL ...
getSymbols("AAPL", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(AAPL)
AAPL_raw <- diff(log(closing_prices))
AAPL_raw <- na.omit(AAPL_raw)
mu_AAPL_LR = mean(AAPL_raw$AAPL.Close[1:604])
mu_AAPL_HR = mean(AAPL_raw$AAPL.Close[605:1340])
#volatility = standard deviation of the returns
sigma_AAPL_LR = sd(AAPL_raw$AAPL.Close[1:604])
sigma_AAPL_HR = sd(AAPL_raw$AAPL.Close[605:1340])
So_AAPL = as.numeric(closing_prices$AAPL.Close[1341,])
So_AAPL

# IV ... Amazon -- AMZN
getSymbols("AMZN", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(AMZN)
closing_prices
AMZN_raw <- diff(log(closing_prices))
AMZN_raw <- na.omit(AMZN_raw)
mu_AMZN_LR = mean(AMZN_raw$AMZN.Close[1:604])
mu_AMZN_HR = mean(AMZN_raw$AMZN.Close[605:1340])
#volatility = standard deviation of the returns
sigma_AMZN_LR = sd(AMZN_raw$AMZN.Close[1:604])
sigma_AMZN_HR = sd(AMZN_raw$AMZN.Close[605:1340])
So_AMZN = as.numeric(closing_prices$AMZN.Close[1341,])
So_AMZN

# V ... Meta Platforms -- META ...
getSymbols("META", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(META)
META_raw <- diff(log(closing_prices))
META_raw <- na.omit(META_raw)
mu_META_LR = mean(META_raw$META.Close[1:604])
mu_META_HR = mean(META_raw$META.Close[605:1340])
#volatility = standard deviation of the returns
sigma_META_LR = sd(META_raw$META.Close[1:604])
sigma_META_HR = sd(META_raw$META.Close[605:1340])
So_META = as.numeric(closing_prices$META.Close[1341,])
So_META

# VI... Broadcom  -- AVGO ...
getSymbols("AVGO", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(AVGO)
AVGO_raw <- diff(log(closing_prices))
AVGO_raw <- na.omit(AVGO_raw)
mu_AVGO_LR = mean(AVGO_raw$AVGO.Close[1:604])
mu_AVGO_HR = mean(AVGO_raw$AVGO.Close[605:1340])
#volatility = standard deviation of the returns
sigma_AVGO_LR = sd(AVGO_raw$AVGO.Close[1:604])
sigma_AVGO_HR = sd(AVGO_raw$AVGO.Close[605:1340])
So_AVGO = as.numeric(closing_prices$AVGO.Close[1341,])
So_AVGO

# VII... Aplhabet Inc. (Class A) -- GOOGL ...
getSymbols("GOOGL", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(GOOGL)
GOOGLa_raw <- diff(log(closing_prices))
GOOGLa_raw <- na.omit(GOOGLa_raw)
mu_GOOGLa_LR = mean(GOOGLa_raw$GOOGL.Close[1:604])
mu_GOOGLa_HR = mean(GOOGLa_raw$GOOGL.Close[605:1340])
#volatility = standard deviation of the returns
sigma_GOOGLa_HR = sd(GOOGLa_raw$GOOGL.Close[605:1340])
sigma_GOOGLa_LR = sd(GOOGLa_raw$GOOGL.Close[1:604])
So_GOOGLa = as.numeric(closing_prices$GOOGL.Close[1341,])
So_GOOGLa

# VIII ... Tesla -- TSLA ...
getSymbols("TSLA", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(TSLA)
TSLA_raw <- diff(log(closing_prices))
TSLA_raw <- na.omit(TSLA_raw)
mu_TSLA_LR = mean(TSLA_raw$TSLA.Close[1:604])
mu_TSLA_HR = mean(TSLA_raw$TSLA.Close[605:1340])
#volatility = standard deviation of the returns
sigma_TSLA_LR = sd(TSLA_raw$TSLA.Close[1:604])
sigma_TSLA_HR = sd(TSLA_raw$TSLA.Close[605:1340])
So_TSLA = as.numeric(closing_prices$TSLA.Close[1341,])
So_TSLA

# XI... Aplhabet Inc. (Class C) -- GOOGL ...

getSymbols("GOOG", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(GOOG)
GOOGLc_raw <- diff(log(closing_prices))
GOOGLc_raw <- na.omit(GOOGLc_raw)
mu_GOOGLc_LR = mean(GOOGLc_raw$GOOG.Close[1:604])
mu_GOOGLc_HR = mean(GOOGLc_raw$GOOG.Close[605:1340])
#volatility = standard deviation of the returns
sigma_GOOGLc_LR = sd(GOOGLc_raw$GOOG.Close[1:604])
sigma_GOOGLc_HR = sd(GOOGLc_raw$GOOG.Close[605:1340])

So_GOOGLc = as.numeric(closing_prices$GOOG.Close[1341,])
So_GOOGLc

# X ... Berkshire Hataway -- BRK.B ...
getSymbols("BRK-B", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")
closing_prices= Cl(`BRK-B`)
BRK_raw <- diff(log(closing_prices))
BRK_raw <- na.omit(BRK_raw)
mu_BRK_LR = mean(BRK_raw$`BRK-B.Close`[1:604])
mu_BRK_HR = mean(BRK_raw$`BRK-B.Close`[605:1340])
#volatility = standard deviation of the returns
sigma_BRK_LR = sd(BRK_raw$`BRK-B.Close`[1:604])
sigma_BRK_HR = sd(BRK_raw$`BRK-B.Close`[605:1340])
So_BRK = as.numeric(closing_prices$`BRK-B.Close`[1341,])
So_BRK

# XI ... Oracle Corporation -- ORCL ...
getSymbols("ORCL", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")

closing_prices= Cl(ORCL)
ORCL_raw <- diff(log(closing_prices))
ORCL_raw <- na.omit(ORCL_raw)
mu_ORCL_LR = mean(ORCL_raw$ORCL.Close[1:604])
mu_ORCL_HR = mean(ORCL_raw$ORCL.Close[605:1340])
#volatility = standard deviation of the returns
sigma_ORCL_LR = sd(ORCL_raw$ORCL.Close[1:604])
sigma_ORCL_HR = sd(ORCL_raw$ORCL.Close[605:1340])
So_ORCL = as.numeric(closing_prices$ORCL.Close[1341,])
So_ORCL

# XXII ... JPMorgam Chase -- JPM ...
getSymbols("JPM", src = "yahoo",
           from = "2020-04-01",
           to   = "2025-08-02")

closing_prices= Cl(JPM)
JPM_raw <- diff(log(closing_prices))
JPM_raw <- na.omit(JPM_raw)
mu_JPM_LR = mean(JPM_raw$JPM.Close[1:604])
mu_JPM_HR = mean(JPM_raw$JPM.Close[605:1340])
#volatility = standard deviation of the returns
sigma_JPM_LR = sd(JPM_raw$JPM.Close[1:604])
sigma_JPM_HR = sd(JPM_raw$JPM.Close[605:1340])
So_JPM = as.numeric(closing_prices$JPM.Close[1341,])
So_JPM

##### Risk Free Rate ####

# US 3 Months Note Bond Yield (short term) -- 4 year span (2020-04-06) to (2025-04-03)

rf_raw = read.csv("DGS3MO.csv")
rf_raw = na.omit(rf_raw)
View(rf_raw)

plot(rf_raw$DGS3MO, main = "",xlab = "Time (days)",ylab = "Yield (%)")
#with index, being the time axis  --> we see a cluster of low and high interest rates. 

plot(density(rf_raw$DGS3MO, na.rm = TRUE),main = "", xlab = "Yield (%)",ylab = "Density")

Mean = mean(rf_raw$DGS3MO, na.rm = TRUE)

#Mean = 2.85

##two interest rate regimes --> High & Low {Change on 2022-08-24}

rf_raw$regime = ifelse(rf_raw$DGS3MO > Mean, "High", "Low")
View(rf_raw)

High_rf_0 = mean(rf_raw$DGS3MO[rf_raw$regime == "High"])
High_rf=High_rf_0/100


Low_rf_0 = mean(rf_raw$DGS3MO[rf_raw$regime == "Low"])
Low_rf=Low_rf_0/100

plot(rf_raw$DGS3MO, col = ifelse(rf_raw$regime == "High", "red", "blue"), lwd = 2,
     main = "",
     ylab = "Yield (%)", xlab = "Time (days)")
abline(h = High_rf_0, col = "red", lwd = 2, lty = 2)
abline(h = Low_rf_0, col = "blue", lwd = 2, lty = 2)

plot(density(rf_raw$DGS3MO[rf_raw$regime == "High"]))

plot(density(rf_raw$DGS3MO[rf_raw$regime == "Low"]))