library(ggplot2)
library(dplyr)
setwd('./compare_results')


func <- function(lake, data_type) {
  full_name = paste(lake,data_type,sep="")
  file_name1 = paste('/Users/yifan.luo/WorkSpace/L2SWBM_v3.202106/results/MonthlyRun_test195001202204_prior1900_10000iteration_0814/',full_name,'_MonthlyRun_test195001202204_prior1900_10000iteration_0814.csv',sep = "")
  file_name2 = paste('~/Desktop/CGLGP_CIA/L2SWBM/GreatLakesWaterBalanceData_19502019/output/ts_posterior/',full_name,'_GLWBData.csv',sep = "")
  csv1 = read.csv(file = file_name1)
  csv2 = read.csv(file = file_name2)
  df1 = data.frame(csv1)
  df1 = df1 %>% 
    mutate(DateTime = as.Date(ISOdate(.$Year, .$Month, 1)))
  df1['category'] = '1'
  
  df2 = data.frame(csv2)
  df2 = df2 %>%
    mutate(DateTime = as.Date(ISOdate(.$Year, .$Month, 1)))
  df2['category'] = '2'
  
  df = df1
  df <- rbind(df1, df2)
  p = ggplot(df, aes(DateTime, Median))
  p +  geom_line(aes(y = Median, colour = category)) +geom_ribbon(aes(ymin = X2.5.Percentile, ymax =X97.5.Percentile), alpha = 0.3)
  
  ggsave(paste(full_name,'.1.pdf',sep=""),p +  geom_line(aes(y = Median, colour = category)) + geom_ribbon(aes(ymin = X2.5.Percentile, ymax =X97.5.Percentile, fill = category), alpha = 0.3)
         , width=15, height=3, units="in", scale=3)
  
  
  
  
  df1 <- df1 %>%
    mutate(uncertainty_percent = (abs(X97.5.Percentile - Median)) / abs(Median))
  
  df1 = df1[df1$uncertainty_percent < 100, ]   
  plot_uncertainty_percent <-
    ggplot(data = df1, aes(x = abs(Median), y = uncertainty_percent)) +
    # geom_line() +
    geom_smooth(method = 'loess',
                colour = "red",
                size = 0.5) +
    geom_point(colour = "black", size = 0.5) 
  
  ggsave(paste(full_name,'.2.pdf',sep=""),plot_uncertainty_percent)
  
  df2 <- df2 %>%
    mutate(uncertainty_percent = (abs(X97.5.Percentile - Median)) / abs(Median))
  
  df2 = df2[df2$uncertainty_percent < 100, ]   
  plot_uncertainty_percent <-
    ggplot(data = df2, aes(x = abs(Median), y = uncertainty_percent)) +
    # geom_line() +
    geom_smooth(method = 'loess',
                colour = "red",
                size = 0.5) +
    geom_point(colour = "black", size = 0.5) 
  
  ggsave(paste(full_name,'.3.pdf',sep=""),plot_uncertainty_percent)
}


for (i in list('erie', 'superior','ontario','miHuron','clair')){
  for (j in list('Diversion', 'DStore','Evap','NBSC','Outflow','Precip','Runoff')){
    try(func(i,j))
  }
  
}