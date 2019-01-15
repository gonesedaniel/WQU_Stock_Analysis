library(quantmod); 
library(TTR);

# Pulling data from Yahoo finance
#Bank of America
symbol = "BAC"
#rm('data_BAC')
data_BAC = loadSymbols(symbol, from ="2000-01-01",auto.assign = FALSE)
colnames(data_BAC) = c("Open","High","Low","Close","Volume","Adjusted")

#Microsoft
symbol = "MSFT"
#rm('data_MSFT')
data_MSFT = loadSymbols(symbol, from ="2000-01-01",auto.assign = FALSE)
colnames(data_MSFT) = c("Open","High","Low","Close","Volume","Adjusted")

#Ford Motor Company
symbol = "F"
#rm('data_F')
data_F = loadSymbols(symbol, from ="2000-01-01",auto.assign = FALSE)
colnames(data_F) = c("Open","High","Low","Close","Volume","Adjusted")

#General Electric
symbol = "E"
#rm('data_E')
data_E = loadSymbols(symbol, from ="2000-01-01",auto.assign = FALSE)
colnames(data_E) = c("Open","High","Low","Close","Volume","Adjusted")

#AT&T
symbol = "T"
#rm('data_T')
data_T = loadSymbols(symbol, from ="2000-01-01",auto.assign = FALSE)
colnames(data_T) = c("Open","High","Low","Close","Volume","Adjusted")


dataStocks = data.frame(data_BAC$Close, dailyReturn(data_BAC$Close, type='log'),
                        data_MSFT$Close, dailyReturn(data_MSFT$Close, type='log'),
                        data_E$Close, dailyReturn(data_E$Close, type='log'),
                        data_F$Close, dailyReturn(data_F$Close, type='log'),
                        data_T$Close, dailyReturn(data_T$Close, type='log'))

colnames(dataStocks) = c("BAC_close","BAC_ret","MSFT_close","MSFT_ret","E_close","E_ret","F_close","F_ret","T_close","T_ret")

dataStocks$Date <- rownames(dataStocks)
dataStocks$Date <- as.Date(dataStocks$Date)
#time plots

library(tidyverse) 
library(ggplot2) 
# view time plots of the closing price  of BAC: 

#ggplot(dataStocks, aes(x = Date)) + 
  #geom_line(aes(y = BAC_close, colour=stocks), colour="blue") + 
  #geom_line(aes(y = MSFT_close), colour = "orange") + 
  #geom_line(aes(y = E_close), colour = "red") + 
  #geom_line(aes(y = F_close), colour = "black") + 
  #geom_line(aes(y = T_close), colour = "green") + 
  #ylab(label="Daily Closing Price") + 
  #xlab("Year") +
  #ggtitle("Time Plots - Stocks") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  #scale_color_manual(values=c("blue", "orange","red","black","green"))

data = dataStocks[,c(1,3,5,7,9,11)]
library(reshape2)
data2 = melt(data, id = c("Date"))

ggplot(data2) + geom_line(aes(x=Date, y=value, colour=variable)) +
  ylab(label="Daily Closing Price") + 
  xlab("Year") +
  ggtitle("Time Plots - Stocks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values=c("blue", "orange","red","black","green"))
  

  ggplot(data = dataStocks) + 
  geom_line(mapping = aes(x = Date, y = BAC_ret),
            color = "darkblue") +labs(x = "Year",y = "Bank of America - Log Daily Return")
  
  ggplot(data = dataStocks) + 
    geom_line(mapping = aes(x = Date, y = MSFT_ret),
              color = "darkorange") +labs(x = "Year",y = "Microsoft - Log Daily Return")
  
  ggplot(data = dataStocks) + 
    geom_line(mapping = aes(x = Date, y = E_ret),
              color = "darkred") +labs(x = "Year",y = "General Electric - Log Daily Return")
  
  ggplot(data = dataStocks) + 
    geom_line(mapping = aes(x = Date, y = F_ret),
              color = "black") +labs(x = "Year",y = "Ford Motor Company - Log Daily Return")
  
  ggplot(data = dataStocks) + 
    geom_line(mapping = aes(x = Date, y = T_ret),
              color = "darkgreen") +labs(x = "Year",y = "AT&T - Log Daily Return")
  
  
  library(kdensity)
  #Bank of America
  #generate histogram with 64 breaks
  
  hist(dataStocks$T_ret, breaks = 64, freq = FALSE, main = "AT&T", xlab = 'daily log returns')
  
  #fit a kernel density estimate starting the search at a normal destribution
  
  kde_BAC = kdensity(dataStocks$T_ret, start = "normal")
  
  #plot the kernel density estimate
  
  lines(kde_BAC, col = "blue")
  
  #plot closest normal distribution
  
  lines(kde_BAC, plot_start = TRUE, col = "red")
  
  #add a legend
  
  legend("topright", c("kernel estimate", "normal"),
         lty = c(1,1), lwd = c(1,1), col = c("blue","red"))
  
  
  #Bank of America
  #compute some of the statistics of the empirical distribution
  
  mean1 = mean(dataStocks$T_ret)
  sd1 = sd(dataStocks$T_ret)
  min1 = min(dataStocks$T_ret)
  max1 = max(dataStocks$T_ret)
  
  #standardize return data to have zero mean and unit variance
  
  BAC_std = (dataStocks$T_ret - mean1)/sd1
  
  #fit quantile quantile plots and reference line going through the 25th and 75th percentile
  
  qqnorm(BAC_std, main = "Normal Q-Q Plot - AT&T",
         plot.it = TRUE, datax = TRUE)
  qqline(BAC_std, datax = FALSE, distribution = qnorm,
         probs = c(0.25, 0.75), qtype = 7)
  
  
  # scatterplot of Microsoft vs Intel log daily returns: 
  
  plot(dataStocks$BAC_ret,dataStocks$MSFT_ret, xlab = "Bank of America", ylab = "Microsoft")
  plot(dataStocks$BAC_ret,dataStocks$T_ret, xlab = "Bank of America", ylab = "AT&T")
  plot(dataStocks$BAC_ret,dataStocks$E_ret, xlab = "Bank of America", ylab = "General Electric")
  plot(dataStocks$BAC_ret,dataStocks$F_ret, xlab = "Bank of America", ylab = "Ford Motor Company")
  
  datastocks_cor = dataStocks[, c(1,3,5,7,9)]
  cor(datastocks_cor)
  
  #stationarity
  



