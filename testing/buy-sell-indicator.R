  library(anytime);
  library(plotly);
  getwd();
  
  # Parameters
  numberOfDays = 2
  
  positiveTrend = 2
  negativeTrend =1
  buyingInterval = 1
  total = 10
  coin = 0 
  
  buyCounter = 0;
  historicalEtherPrice = read.csv("./etherprice.csv");
  
  recency = nrow(historicalEtherprice) - 50 #past 50 days
  
  
  #define extra columns
  historicalEtherPrice$historicalAverage <- NA
  historicalEtherPrice$historicalGradient <- NA
  historicalEtherPrice$change <- NA
  historicalEtherPrice$coin <-NA
  historicalEtherPrice$longTermGradient <-NA
  
  historicalEtherPrice$coin[1] <- coin
  historicalEtherPrice$total <-NA
  historicalEtherPrice$total[1] <- total
  historicalEtherPrice[is.na(historicalEtherPrice)] <- 0;
 

  for (i in (numberOfDays+1):nrow(historicalEtherPrice)){
    
    historicalEtherPrice$historicalAverage[i] <- colMeans(subset(historicalEtherPrice, time <= historicalEtherPrice$time[i] & time >= historicalEtherPrice$time[(i-(numberOfDays))] , select = c(price), na.rm = FALSE));
    historicalEtherPrice$historicalGradient[i] <- ((historicalEtherPrice$price[i] - historicalEtherPrice$price[(i-numberOfDays)])/numberOfDays);
    historicalEtherPrice$change[i] <- ((historicalEtherPrice$price[i] - historicalEtherPrice$price[(i-1)]));

  }
 
  


  
  # Run stats
  for (i in (2):nrow(historicalEtherPrice)){
    
  priceIncrease = subset(historicalEtherPrice, time <= historicalEtherPrice$time[i] & time >= historicalEtherPrice$time[(i-(positiveTrend))] , select = c(historicalGradient), na.rm = FALSE)[[1]];
  priceDecrease = subset(historicalEtherPrice, time <= historicalEtherPrice$time[i] & time >= historicalEtherPrice$time[(i-(negativeTrend))] , select = c(historicalGradient), na.rm = FALSE)[[1]];
  
  if(all(priceIncrease > 0)){
    if (coin > 0) {
      total = total + (coin * historicalEtherPrice$price[i]);
      coin = 0
      historicalEtherPrice$coin[i] <- coin; 
      historicalEtherPrice$total[i] <- total;
      buyCounter= 0
    } else {
      historicalEtherPrice$coin[i] <- historicalEtherPrice$coin[i-1] 
      historicalEtherPrice$total[i] <- historicalEtherPrice$total[i-1] 
    }
  } else if(all(priceDecrease < 0 & buyCounter < 2) ) {
    coin = buyingInterval/historicalEtherPrice$price[i] 
    total = total - buyingInterval;
    historicalEtherPrice$coin[i] <- coin; 
    historicalEtherPrice$total[i] <- total;
    buyCounter = buyCounter + 1;
  }else{
    historicalEtherPrice$coin[i] <- historicalEtherPrice$coin[i-1] 
    historicalEtherPrice$total[i] <- historicalEtherPrice$total[i-1] 
  }
  }
  
  total
  coin



# 
# HistoricalPrice <- plot_ly(historicalEtherPrice, x = ~historicalEtherPrice$time, y = ~historicalEtherPrice$price, name = 'Price', type = 'scatter', mode = 'lines')%>%
#   add_trace(y = ~historicalEtherPrice$historicalAverage, name = '30 Day average', mode = 'lines+markers');
# 
# HistoricalGradient<- plot_ly(historicalEtherPrice, x = ~historicalEtherPrice$time, y = ~historicalEtherPrice$historicalGradient, name = 'Historical 30 day gradient', type = 'scatter', mode = 'lines')%>%
#   add_trace(y = ~historicalEtherPrice$change, name = '30 Day average', mode = 'lines+markers');
# 
#  DailyGradient <- plot_ly(historicalEtherPrice, x = ~historicalEtherPrice$time, y = ~historicalEtherPrice$change, name = 'Historical 30 day gradient', type = 'scatter', mode = 'lines')
#  
  
  
 modelPriceCoin<- plot_ly(historicalEtherPrice, x = ~historicalEtherPrice$time, y = ~historicalEtherPrice$historicalGradient, name = 'Historical historical gradient', type = 'scatter', mode = 'lines')%>%
   add_trace(y = ~historicalEtherPrice$coin, name = 'coin', mode = 'lines+markers') %>%
 add_trace(y = ~historicalEtherPrice$total, name = 'total', mode = 'lines+markers');
 
 modelPriceCoin
  
# HistoricalGradient
# 
# testFrame


