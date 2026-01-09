####################### SMALL CAP ##########################
###### WEIGHTS ######   

# (use 10'000'000)

##### FOR High Interest Rate Regime ####

####      A - Small-Equality Weighted Portfolio     ####

# SEV --> Small Equally Weighted  AA

# with 12 stocks 
1/12
#= 0.08333333 --> 8.333333%
#so 8.333333% in each stock 
# => 10'000'000 * 8.333333% = 833'333.333 in each stock 

Table_SEW_HR = data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So = Portfolio_Small_Info$Stock_So,
  Numb_Stocks = round(833333.333 / Portfolio_Small_Info$Stock_So)
)

#View(Table_SEW_HR)


sum(Table_SEW_HR$Numb_Stocks * Table_SEW_HR$So)
#= 10000041
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio




####      B - Small-Volatitity Weighted Portfolio     ####

# SVW --> Small Volatility Weighted  BB

# w =  ((vol) / (∑ vol))

So = Portfolio_Small_Info$Stock_So
Sigma= Portfolio_Small_Info$Stock_Sigma_HR
Wi= (Sigma) / sum(Sigma)
Wi



Table_SVW_HR = data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So = Portfolio_Small_Info$Stock_So,
  Numb_Stocks= round((10000000 * Wi) / Portfolio_Small_Info$Stock_So)
)
#View(Table_SVW_HR)


sum(Table_SVW_HR$Numb_Stocks * Table_SVW_HR$So)


#= 10000008
# not exactly 10'000'000$ but the closest approximation we can reach with an equally weighted portfolio



####      C - Small-Inverse Volatitity Weighted Portfolio     ####

# SIW --> Small Inverserly Weighted  CC

# W = (1/vol) / (∑ 1/vol)

So = Portfolio_Small_Info$Stock_So
Sigma= Portfolio_Small_Info$Stock_Sigma_HR
Wi= (1 / Sigma) / sum(1 / Sigma)

Table_SIW_HR <- data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So =So,
  Numb_Stocks= round((10000000 * Wi) / So)
)

#View(Table_SIW_HR)


sum(Table_SIW_HR$Numb_Stocks * Table_SIW_HR$So)

#= 9999984
# not exactly 10'000'000$ but the closest approximation we can reach with an equally weighted portfolio







###### FOR LOW Interest Rate Regime #####


####      A - Small -Equality Weighted Portfolio     ####

# LEV --> Small Equally Weighted  AA

# with 12 stocks 
1/12
#= 0.08333333 --> 8.333333%
#so 8.333333% in each stock 
# => 10'000'000 * 8.333333% = 833'3333.333 in each stock 

Table_SEW_LR = data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So = Portfolio_Small_Info$Stock_So,
  Numb_Stocks = round(833333.333 / Portfolio_Small_Info$Stock_So)
)

#View(Table_SEW_LR)

sum(Table_SEW_LR$Numb_Stocks * Table_SEW_LR$So)
#= 10000041
# not exactly 10'00'000$ but the closest approximation we can reach with an equally weighted portfolio




####      B - Small-Volatitity Weighted Portfolio     ####

# LVW --> Small Volatility Weighted  BB

# w =  ((vol) / (∑ vol))


So = Portfolio_Small_Info$Stock_So
Sigma= Portfolio_Small_Info$Stock_Sigma_LR
Wi= (Sigma) / sum(Sigma)

Table_SVW_LR = data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So = Portfolio_Small_Info$Stock_So,
  Numb_Stocks= round((10000000 * Wi) / Portfolio_Small_Info$Stock_So)
)
#View(Table_SVW_LR)


sum(Table_SVW_LR$Numb_Stocks * Table_SVW_LR$So)


#= 10000038 
# not exactly 10'000'000$ but the closest approximation we can reach with an equally weighted portfolio



####      C - Small-Inverse Volatitity Weighted Portfolio     ####

# SIW --> Small Inverserly Weighted  CC

# W = (1/vol) / (∑ 1/vol)

So = Portfolio_Small_Info$Stock_So
Sigma= Portfolio_Small_Info$Stock_Sigma_LR
Wi= (1 / Sigma) / sum(1 / Sigma)


Table_SIW_LR <- data.frame(
  Ticker = Portfolio_Small_Info$Stock_Ticker,
  So =So,
  Numb_Stocks= round((10000000 * Wi) / So)
)

#View(Table_SIW_LR)


sum(Table_SIW_LR$Numb_Stocks * Table_SIW_LR$So)

#= 10000010 
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio




################# LARGE CAP ######################

###### WEIGHTS ######   

# (use 10'000'000)

##### FOR High Interest Rate Regime ####

####      A - Large-Equality Weighted Portfolio     ####

# LEV --> Large Equally Weighted  AA

# with 12 stocks 
1/12
#= 0.08333333 --> 8.333333%
#so 8.333333% in each stock 
# => 10'000'000 * 8.333333% = 833'333.333 in each stock 

Table_LEW_HR = data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So = Portfolio_Large_Info$Stock_So,
  Numb_Stocks = round(833333.333 / Portfolio_Large_Info$Stock_So)
)

#View(Table_LEW_HR)


sum(Table_LEW_HR$Numb_Stocks * Table_LEW_HR$So)
#= 9999683
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio




####      B - Large-Volatitity Weighted Portfolio     ####

# LVW --> Large Volatility Weighted  BB

# w =  ((vol) / (∑ vol))



Sigma= Portfolio_Large_Info$Stock_Sigma_HR
So = Portfolio_Large_Info$Stock_So
Wi= (Sigma) / sum(Sigma)


Table_LVW_HR = data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So = Portfolio_Large_Info$Stock_So,
  Numb_Stocks= round((10000000 * Wi) / Portfolio_Large_Info$Stock_So)
)
#View(Table_LVW_HR)


sum(Table_LVW_HR$Numb_Stocks * Table_LVW_HR$So)


#= 10000287
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio



####      C - Large-Inverse Volatitity Weighted Portfolio     ####

# LIW --> Large Inverserly Weighted  CC

# W = (1/vol) / (∑ 1/vol)


So = Portfolio_Large_Info$Stock_So
Sigma= Portfolio_Large_Info$Stock_Sigma_HR
Wi= (1 / Sigma) / sum(1 / Sigma)


Table_LIW_HR <- data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So =So,
  Numb_Stocks= round((10000000 * Wi) / So)
)

#View(Table_LIW_HR)


sum(Table_LIW_HR$Numb_Stocks * Table_LIW_HR$So)

#= 9999893
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio



###### FOR LOW Interest Rate Regime #####


####      A - Large -Equality Weighted Portfolio     ####

# LEV --> Large Equally Weighted  AA

# with 12 stocks 
1/12
#= 0.08333333 --> 8.333333%
#so 8.333333% in each stock 
# => 100'000 * 8.333333% = 8'3333.333 in each stock 

Table_LEW_LR = data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So = Portfolio_Large_Info$Stock_So,
  Numb_Stocks = round(833333.333 / Portfolio_Large_Info$Stock_So)
)

#View(Table_LEW_LR)

sum(Table_LEW_LR$Numb_Stocks * Table_LEW_LR$So)
#= 9999683
# not exactly 100'000$ but the closest approximation we can reach with an equally weighted portfolio




####      B - Large-Volatitity Weighted Portfolio     ####

# LVW --> Large Volatility Weighted  BB

# w =  ((vol) / (∑ vol))



Sigma= Portfolio_Large_Info$Stock_Sigma_LR
Portfolio_Large_Info$Stock_So
Wi= (Sigma) / sum(Sigma)


Table_LVW_LR = data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So = Portfolio_Large_Info$Stock_So,
  Numb_Stocks= round((10000000 * Wi) / Portfolio_Large_Info$Stock_So)
)
#View(Table_LVW_LR)


sum(Table_LVW_LR$Numb_Stocks * Table_LVW_LR$So)


#= 9999473 
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio



####      C - Large-Inverse Volatitity Weighted Portfolio     ####

# LIW --> Large Inverserly Weighted  CC

# W = (1/vol) / (∑ 1/vol)



So = Portfolio_Large_Info$Stock_So


Sigma= Portfolio_Large_Info$Stock_Sigma_LR
Wi= (1 / Sigma) / sum(1 / Sigma)


Table_LIW_LR <- data.frame(
  Ticker = Portfolio_Large_Info$Stock_Ticker,
  So =So,
  Numb_Stocks= round((10000000 * Wi) / So)
)

#View(Table_LIW_LR)


sum(Table_LIW_LR$Numb_Stocks * Table_LIW_LR$So)

#= 9999560 
# not exactly 1'000'000$ but the closest approximation we can reach with an equally weighted portfolio


