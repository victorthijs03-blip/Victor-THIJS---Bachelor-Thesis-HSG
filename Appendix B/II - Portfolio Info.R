## tables 

##### PORTFOLIO SMALL (small stock) ####

Portfolio_Small_Info = data.frame(Stock_Names= c('AAR Corp.', 'Standex International Corp', 'Beacon Financial Corp.','Supernus Pharm', 'Biolife Solutions','Adeia Inc','Iac Inc', 'Customers Bancorp','Bancfirst Corp','Gibraltar Ind Inc','Tootsie Roll Industries','Vir Biotechnology Inc ' ), 
                                  Stock_Ticker= c('AIR', 'SXI', 'BRKL', 'SUPN','BLFS','ADEIA', 'IAC','CBKM', 'BANF', ' ROCK', 'TR', 'VIR' ), 
                                  Stock_So= c(So_AAR, So_SXI, So_BRKL, So_SUPN, So_BLFS, So_ADEIA, So_IAC, So_CBKM, So_BANF, So_ROCK, So_TR, So_VIR), 
                                  Stock_MU_HR=c(mu_AAR_HR, mu_SXI_HR, mu_BRKL_HR, mu_SUPN_HR, mu_BLFS_HR, mu_ADEIA_HR, mu_IAC_HR, mu_CBKM_HR, mu_BANF_HR, mu_ROCK_HR, mu_TR_HR, mu_VIR_HR ), 
                                  Stock_MU_LR=c(mu_AAR_LR, mu_SXI_LR, mu_BRKL_LR, mu_SUPN_LR, mu_BLFS_LR, mu_ADEIA_LR, mu_IAC_LR, mu_CBKM_LR, mu_BANF_LR, mu_ROCK_LR, mu_TR_LR, mu_VIR_LR ), 
                                  Stock_Mu_in_percentage_HR= c(100*mu_AAR_HR, 100*mu_SXI_HR, 100*mu_BRKL_HR, 100*mu_SUPN_HR, 100*mu_BLFS_HR, 100*mu_ADEIA_HR, 100*mu_IAC_HR, 100*mu_CBKM_HR, 100*mu_BANF_HR, 100*mu_ROCK_HR, 100*mu_TR_HR, 100*mu_VIR_HR), 
                                  Stock_Mu_in_percentage_LR= c(100*mu_AAR_LR, 100*mu_SXI_LR, 100*mu_BRKL_LR, 100*mu_SUPN_LR, 100*mu_BLFS_LR, 100*mu_ADEIA_LR, 100*mu_IAC_LR, 100*mu_CBKM_LR, 100*mu_BANF_LR, 100*mu_ROCK_LR, 100*mu_TR_LR, 100*mu_VIR_LR),
                                  Stock_Sigma_HR= c(sigma_AAR_HR, sigma_SXI_HR, sigma_BRKL_HR, sigma_SUPN_HR, sigma_BLFS_HR, sigma_ADEIA_HR, sigma_IAC_HR, sigma_CBKM_HR, sigma_BANF_HR, sigma_ROCK_HR, sigma_TR_HR, sigma_VIR_HR), 
                                  Stock_Sigma_LR= c(sigma_AAR_LR, sigma_SXI_LR, sigma_BRKL_LR, sigma_SUPN_LR, sigma_BLFS_LR, sigma_ADEIA_LR, sigma_IAC_LR, sigma_CBKM_LR, sigma_BANF_LR, sigma_ROCK_LR, sigma_TR_LR, sigma_VIR_LR),
                                  Stock_Sigma_in_percentage_HR= c(100*sigma_AAR_HR, 100*sigma_SXI_HR, 100*sigma_BRKL_HR, 100*sigma_SUPN_HR, 100*sigma_BLFS_HR, 100*sigma_ADEIA_HR, 100*sigma_IAC_HR, 100*sigma_CBKM_HR, 100*sigma_BANF_HR, 100*sigma_ROCK_HR, 100*sigma_TR_HR, 100*sigma_VIR_HR),
                                  Stock_Sigma_in_percentage_LR= c(100*sigma_AAR_LR, 100*sigma_SXI_LR, 100*sigma_BRKL_LR, 100*sigma_SUPN_LR, 100*sigma_BLFS_LR, 100*sigma_ADEIA_LR, 100*sigma_IAC_LR, 100*sigma_CBKM_LR, 100*sigma_BANF_LR, 100*sigma_ROCK_LR, 100*sigma_TR_LR, 100*sigma_VIR_LR)
)
View(Portfolio_Small_Info)



##### PORTFOLIO LARGE (large stock) ####

Portfolio_Large_Info = data.frame(Stock_Names= c('Nvidia Corp.', 'Microsoft Corp', 'Apple Inc.','Amazon.com Inc','Meta Platforms Inc.', 'Broadcom Inc.', 'Alphabet Inc. (class A) ','Tesla Inc.','Alphabet Inc. (class C)','Berkshire Hathaway','Oracle Corporation', 'Jpmorgan Chase & Co' ), 
                                  Stock_Ticker= c('NVDA', 'MSFT', 'AAPL','AMZN', 'META', 'AVGO', 'GOOGL A', 'TSLA', 'GOOGL C','BRK', 'ORCL','JPM' ),
                                  Stock_So= c(So_NVDA,So_MSFT, So_AAPL, So_AMZN,So_META, So_AVGO, So_GOOGLa, So_TSLA, So_GOOGLc, So_BRK, So_ORCL, So_JPM), 
                                  Stock_MU_HR=c(mu_NVDA_HR,mu_MSFT_HR, mu_AAPL_HR, mu_AMZN_HR, mu_META_HR, mu_AVGO_HR, mu_GOOGLa_HR, mu_TSLA_HR, mu_GOOGLc_HR, mu_BRK_HR, mu_ORCL_HR, mu_JPM_HR ), 
                                  Stock_MU_LR=c(mu_NVDA_LR,mu_MFST_LR, mu_AAPL_LR, mu_AMZN_LR, mu_META_LR, mu_AVGO_LR, mu_GOOGLa_LR, mu_TSLA_LR, mu_GOOGLc_LR, mu_BRK_LR, mu_ORCL_LR, mu_JPM_LR), 
                                  Stock_Mu_in_percentage_HR= c(100*mu_NVDA_HR, 100*mu_MSFT_HR, 100*mu_AAPL_HR, 100*mu_AMZN_HR, 100*mu_META_HR, 100*mu_AVGO_HR, 100*mu_GOOGLa_HR, 100*mu_TSLA_HR, 100*mu_GOOGLc_HR,mu_BRK_HR*100,mu_ORCL_HR*100,mu_JPM_HR*100  ),
                                  Stock_Mu_in_percentage_LR= c(100*mu_NVDA_LR, 100*mu_MFST_LR, 100*mu_AAPL_LR, 100*mu_AMZN_LR, 100*mu_META_LR, 100*mu_AVGO_LR, 100*mu_GOOGLa_LR, 100*mu_TSLA_LR, 100*mu_GOOGLc_LR,mu_BRK_LR*100,mu_ORCL_LR*100,mu_JPM_LR*100   ),
                                  Stock_Sigma_HR= c(sigma_NVDA_HR, sigma_MSFT_HR, sigma_AAPL_HR, sigma_AMZN_HR, sigma_META_HR,sigma_AVGO_HR, sigma_GOOGLa_HR, sigma_TSLA_HR, sigma_GOOGLc_HR,sigma_BRK_HR, sigma_ORCL_HR, sigma_JPM_HR ),
                                  Stock_Sigma_LR= c(sigma_NVDA_LR, sigma_MSFT_LR, sigma_AAPL_LR, sigma_AMZN_LR, sigma_META_LR,sigma_AVGO_LR, sigma_GOOGLa_LR,sigma_TSLA_LR, sigma_GOOGLc_LR, sigma_BRK_LR, sigma_ORCL_LR, sigma_JPM_LR),
                                  Stock_Sigma_in_percentage_HR= c(100*sigma_NVDA_HR, 100*sigma_MSFT_HR, sigma_AAPL_HR*100,100*sigma_AMZN_HR, 100*sigma_META_HR, 100*sigma_AVGO_HR, 100*sigma_GOOGLa_HR, 100*sigma_TSLA_HR, 100*sigma_GOOGLc_HR, 100*sigma_BRK_HR, 100*sigma_ORCL_HR, 100*sigma_JPM_HR ),
                                  Stock_Sigma_in_percentage_LR= c(100*sigma_NVDA_LR, 100*sigma_MSFT_LR, sigma_AAPL_LR*100,100*sigma_AMZN_LR, 100*sigma_META_LR, 100*sigma_AVGO_LR, 100*sigma_GOOGLa_LR, 100*sigma_TSLA_LR, 100*sigma_GOOGLc_LR, 100*sigma_BRK_LR, 100*sigma_ORCL_LR, 100*sigma_JPM_LR)
)

#View(Portfolio_Large_Info)




###### For Discussions purposes ###### --> see also Appendix A


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




