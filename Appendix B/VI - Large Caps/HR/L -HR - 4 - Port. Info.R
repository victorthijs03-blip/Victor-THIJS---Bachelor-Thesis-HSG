
##### INDEX  ##### 

mean(Index_Simulation_L_HR[, 64])
Index_Simulation_L_HR[1, 1]

# Number of paths below strike levels
sum(Index_Simulation_L_HR[, 64] < (0.90 * Index_Simulation_L_HR[1, 1]))
sum(Index_Simulation_L_HR[, 64] < (0.95 * Index_Simulation_L_HR[1, 1]))
sum(Index_Simulation_L_HR[, 64] < (1.00 * Index_Simulation_L_HR[1, 1]))
sum(Index_Simulation_L_HR[, 64] < (1.05 * Index_Simulation_L_HR[1, 1]))

# Plot first 50 Index paths
matplot(
  t(Index_Simulation_L_HR[1:50, ]),
  type = "l", lty = 1, col = "black",
  xlab = "Days", ylab = "Index_Simulation_L_HR Value",
  main = "First 50 Simulated Index Paths (Merton JD)"
)
abline(h = Index_Simulation_L_HR[1, 1], col = "red", lwd = 2, lty = 2)

# Mean Index path
mean_path_index <- colMeans(Index_Simulation_L_HR)
plot(
  mean_path_index,
  type = "l", lwd = 2, col = "blue",
  xlab = "Days", ylab = "Index Value",
  main = "Mean Simulated Index Path"
)
abline(h = Index_Simulation_L_HR[1, 1], col = "red", lwd = 2, lty = 2)



##### EQUALLY WEIGHTED PORTFOLIO #####


matplot(
  t(Portfolio_0_S0_L_EW_HR[1:50, ]),
  type = "l", lty = 1, col = "black",
  xlab = "Days", ylab = "Portfolio Value",
  main = "First 50 Simulated Paths – Large Portfolio (EW, HR)"
)
abline(h = Portfolio_0_S0_L_EW_HR[1, 1], col = "red", lwd = 2, lty = 2)

sum(Portfolio_0_S0_L_EW_HR[, 64] < Portfolio_0_S0_L_EW_HR[1, 1])
mean(Portfolio_0_S0_L_EW_HR[, 64])
min(Portfolio_0_S0_L_EW_HR[, 64])

hist(
  Portfolio_0_S0_L_EW_HR[, 64],
  breaks = 100, col = "skyblue", border = "white",
  main = "Terminal Distribution – EW Portfolio (63 days)",
  xlab = "Portfolio Value", ylab = "Frequency"
)
abline(v = Portfolio_0_S0_L_EW_HR[1, 1], col = "red", lwd = 2, lty = 2)



##### VOLATILITY WEIGHTED PORTFOLIO #####


matplot(
  t(Portfolio_0_S0_L_VW_HR[1:50, ]),
  type = "l", lty = 1, col = "black",
  xlab = "Days", ylab = "Portfolio Value",
  main = "First 50 Simulated Paths – Large Portfolio (VW, HR)"
)
abline(h = Portfolio_0_S0_L_VW_HR[1, 1], col = "red", lwd = 2, lty = 2)

sum(Portfolio_0_S0_L_VW_HR[, 64] < Portfolio_0_S0_L_VW_HR[1, 1])
mean(Portfolio_0_S0_L_VW_HR[, 64])
min(Portfolio_0_S0_L_VW_HR[, 64])

hist(
  Portfolio_0_S0_L_VW_HR[, 64],
  breaks = 100, col = "skyblue", border = "white",
  main = "Terminal Distribution – VW Portfolio (63 days)",
  xlab = "Portfolio Value", ylab = "Frequency"
)
abline(v = Portfolio_0_S0_L_VW_HR[1, 1], col = "red", lwd = 2, lty = 2)



##### INVERSE VOLATILITY WEIGHTED #####


matplot(
  t(Portfolio_0_S0_L_IW_HR[1:50, ]),
  type = "l", lty = 1, col = "black",
  xlab = "Days", ylab = "Portfolio Value",
  main = "First 50 Simulated Paths – Large Portfolio (IW, HR)"
)
abline(h = Portfolio_0_S0_L_IW_HR[1, 1], col = "red", lwd = 2, lty = 2)

sum(Portfolio_0_S0_L_IW_HR[, 64] < Portfolio_0_S0_L_IW_HR[1, 1])
mean(Portfolio_0_S0_L_IW_HR[, 64])
min(Portfolio_0_S0_L_IW_HR[, 64])

hist(
  Portfolio_0_S0_L_IW_HR[, 64],
  breaks = 100, col = "skyblue", border = "white",
  main = "Terminal Distribution – IW Portfolio (63 days)",
  xlab = "Portfolio Value", ylab = "Frequency"
)
abline(v = Portfolio_0_S0_L_IW_HR[1, 1], col = "red", lwd = 2, lty = 2)
