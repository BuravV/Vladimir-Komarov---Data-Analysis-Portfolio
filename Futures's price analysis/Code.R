################################################################################

#packages 

library(readxl) 
library(lubridate)  
library(moments) 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rugarch)
library(tidyverse)
library(tsoutliers)
library(tseries)

#functions

data_trasnfrom <- function(data1,data2){
  
  #union data 
  data <- rbind(data1, data2)
  
  #turning date to actual date format 
  data$Date <- mdy(data$Date)
  
  #cleaning rate of return 
  data$Change.. <- as.numeric(gsub("%", "", data$Change..)) / 100
  colnames(data)[7]  <- "ReturnRate"
  
  #dropping unwanted columns
  data <- select(data, Date, Price,ReturnRate)
  
  #sorting data
  data <- data %>% arrange(Date)
  
  #filtering data 
  data <- subset(data, data$Date >= as.Date("2010-01-01"))
  row.names(data) <- NULL 
  
  return(data)
  
}

MSE_cal <- function(model){
  
  e1 = residuals(model)
  d1 = e1^2 - sigma(model)^2
  mse <- mean(d1^2)
  
  return(mse)
  
}

sigma_data <- function(s_Garch, s_djrGarch,s_Egarch, type){ 
  
  #GARCH sigma 
  sigma_col <- as.numeric(s_Garch[,1])
  pred_sigma_df <- data.frame(PredVolatility = sigma_col)
  pred_sigma_df['type'] = "GARCH"
  
  #GJR_GARCH sigma 
  sigma_col2 <- as.numeric(s_djrGarch[,1])
  pred_sigma_df2 <- data.frame(PredVolatility = sigma_col2)
  pred_sigma_df2['type'] = ".GJR-GARCH"
  
  #EGARCH sigma 
  sigma_col3 <- as.numeric(s_Egarch[,1])
  pred_sigma_df3 <- data.frame(PredVolatility = sigma_col3)
  pred_sigma_df3['type'] = "EGARCH"
  
  #Real sigma 
  if (type == "oil") {
    pred_sigma_df4 <- data.frame(PredVolatility = (oil$ReturnRate))
    pred_sigma_df4['type'] = "REAL" 
    date_col <- oil$Date
  } else if (type == "gas") {
    pred_sigma_df4 <- data.frame(PredVolatility = (gas$ReturnRate))
    pred_sigma_df4['type'] = "REAL"
    date_col <- gas$Date
  } else {
    stop("Invalid input")
  }
  
  #Combining with date 
  pred_sigma_df <- cbind(pred_sigma_df, Date = date_col)
  
  pred_sigma_df2 <- cbind(pred_sigma_df2, Date = date_col)
  
  pred_sigma_df3 <- cbind(pred_sigma_df3, Date = date_col)
  
  pred_sigma_df4 <- cbind(pred_sigma_df4, Date = date_col)
  
  #Combining all together 
  combined <- do.call("rbind", list(pred_sigma_df, pred_sigma_df2,pred_sigma_df3))
  
  return(combined)
  
}


################################################################################

#OIL DATA  

Oildata1 <- read.csv("Brent Oil Futures Historical Data (part1).csv")
Oildata2 <- read.csv("Brent Oil Futures Historical Data (part2).csv")

oil <- data_trasnfrom(Oildata1,Oildata2)
count(oil)

#GAS DATA 

Gasdata1 <- read.csv("Natural Gas Futures Historical Data (part1).csv")
Gasdata2 <- read.csv("Natural Gas Futures Historical Data (part2).csv")

gas <- data_trasnfrom(Gasdata1,Gasdata2)
count(gas)

################################################################################

#Statistics 

statOil <- c(mean(oil$ReturnRate),sd(oil$ReturnRate),skewness(oil$ReturnRate),
             kurtosis(oil$ReturnRate))
statOil

statGas <- c(mean(gas$ReturnRate), sd(gas$ReturnRate), skewness(gas$ReturnRate), 
             kurtosis(gas$ReturnRate))
statGas

################################################################################

#Graphs 

#Oil price 

g_oil_price <- ggplot(oil, aes(x = Date, y = Price, group = 1)) + geom_line() +
  labs(x = "", y = "US dollars") + scale_x_date(date_labels  = "%Y",date_breaks = "1 years") + 
  theme_light() + labs(title ='Brent crude oil') + theme(axis.text.x = element_text(angle = 45, hjust = 1)
                                                         ,plot.title = element_text(hjust = 0.5, vjust = 1))

g_oil_price

#Gas prices 

g_gas_price <- ggplot(gas, aes(x = Date, y = Price, group = 1)) + geom_line() +
  labs(x = "", y = "") + scale_x_date(date_labels  = "%Y",date_breaks = "1 years") +  
  theme_light() + labs(title = 'Natural gas') + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                       plot.title = element_text(hjust = 0.5, vjust = 1))
                                              
g_gas_price

#combining graphs 
combined_plot <- grid.arrange(g_oil_price,g_gas_price, ncol = 2)


#Oil Return 

g_oil_return <- ggplot(oil, aes(x = Date, y = ReturnRate, group = 1)) + geom_line() +
  labs(x = "", y = "") + scale_x_date(date_labels  = "%Y",date_breaks = "1 years") +
        theme_light() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

g_oil_return

dens_oil <- ggplot(oil, aes(x = ReturnRate)) +
  geom_histogram(aes(y = after_stat(density), fill = "Actual"), binwidth = 0.006, alpha = 0.8, color = "grey") +
  stat_function(fun = dnorm, args = list(mean = mean(oil$ReturnRate), sd = sd(oil$ReturnRate)), 
                aes(color = "Normal"), size = 1.2, alpha = 0.7) +
  scale_color_manual(name = "", values = "red") +
  scale_fill_manual(name = "", values = c("Actual" = "grey")) +
  labs(x = "", y = "Density") +
  theme_light() +
  theme(legend.position = c(0.88, 0.85),
        legend.background = element_blank(),
        legend.key.size = unit(0.5, "lines"),
        legend.spacing.y = unit(0.000002, "lines"))

dens_oil

#combining graphs 
combined_plot_oil <- grid.arrange(g_oil_return,dens_oil, ncol = 2)


#Gas Return

g_gas_return <- ggplot(gas, aes(x = Date, y = ReturnRate, group = 1)) + geom_line() +
  labs(x = "", y = "") + scale_x_date(date_labels  = "%Y",date_breaks = "1 years") + theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
g_gas_return

dens_gas <- ggplot(gas, aes(x = ReturnRate)) +
  geom_histogram(aes(y = after_stat(density), fill = "Actual"), binwidth = 0.006, alpha = 0.8, color = "grey") +
  stat_function(fun = dnorm, args = list(mean = mean(gas$ReturnRate), sd = sd(gas$ReturnRate)), 
                aes(color = "Normal"), size = 1.2, alpha = 0.7) +
  scale_color_manual(name = "", values = "red") +
  scale_fill_manual(name = "", values = c("Actual" = "grey")) +
  labs(x = "", y = "Density") +
  theme_light() +
  theme(legend.position = c(0.85, 0.8),
        legend.background = element_blank(),
        legend.key.size = unit(0.5, "lines"),
        legend.spacing.y = unit(0.000002, "lines"))

dens_gas

combined_plot_gas <- grid.arrange(g_gas_return,dens_gas, ncol = 2)


################################################################################

### MODELS 

#GARCH 
garchspecN <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"), 
                        distribution.model = "std")

#GJR-GARCH  
garchspecGJR <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                           variance.model = list(model = "gjrGARCH"), 
                           distribution.model = "std")

#EGARCH 
garchspecE<- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "eGARCH"), 
                        distribution.model = "std")

#Model fitting (OIL)

#GARCH 

#Info 
garchfitN <- ugarchfit(data = oil$ReturnRate, spec = garchspecN)
coef(garchfitN)  
round(garchfitN@fit$matcoef,6)
infocriteria(garchfitN)

#MSE 
MSE_cal(garchfitN)

#DJR-GARCH 

#Info
garchfitGJR <- ugarchfit(data = oil$ReturnRate, spec = garchspecGJR)
coef(garchfitGJR)
round(garchfitGJR@fit$matcoef,6)
infocriteria(garchfitGJR)

#MSE 
MSE_cal(garchfitGJR)

#EGARCH

#Info
garchfitE <- ugarchfit(data = oil$ReturnRate, spec = garchspecE)
coef(garchfitE)
round(garchfitE@fit$matcoef,6)
infocriteria(garchfitE)

#MSE
MSE_cal(garchfitE)


#Sigma fitting 

sigma_garch <- sigma(garchfitN)
sigma_garchGJR <- sigma(garchfitGJR)
sigma_garchE <- sigma(garchfitE)

combined_oil <- sigma_data(sigma_garch,sigma_garchGJR,sigma_garchE, type = "oil")

#graph 
model_com <- ggplot(oil, aes(x = Date, y = ReturnRate, group = 1)) + geom_line(color = "grey",alpha = 1) + 
  geom_line(data = combined_oil, aes(x = Date, y = PredVolatility, group = type, color=type))+ scale_x_date(date_labels  = "%Y",date_breaks = "1 years") + 
  theme_light() + theme(legend.position = c(0.1, 0.2))+ xlab(NULL)+ylab(NULL) + labs(color = "Model")

model_com

#Model fitting (GAS)

#Info
garchfitN <- ugarchfit(data = gas$ReturnRate, spec = garchspecN)
coef(garchfitN)  
round(garchfitN@fit$matcoef,6)
infocriteria(garchfitN)

#MSE 
MSE_cal(garchfitN)

#DJR-GARCH 

#Info
garchfitGJR <- ugarchfit(data = gas$ReturnRate, spec = garchspecGJR)
coef(garchfitGJR)
round(garchfitGJR@fit$matcoef,6)
infocriteria(garchfitGJR)

#MSE 
MSE_cal(garchfitGJR)

#EGARCH

#Info
garchfitE <- ugarchfit(data = gas$ReturnRate, spec = garchspecE)
coef(garchfitE)
round(garchfitE@fit$matcoef,6)
infocriteria(garchfitE)

#MSE 
MSE_cal(garchfitE)

sigma_garch <- sigma(garchfitN)
sigma_garchGJR <- sigma(garchfitGJR)
sigma_garchE <- sigma(garchfitE)

combined_gas <- sigma_data(sigma_garch,sigma_garchGJR,sigma_garchE, type = "gas")

#graph 
model_com_gas <- ggplot(gas, aes(x = Date, y = ReturnRate, group = 1)) + geom_line(color = "grey",alpha = 1) + 
  geom_line(data = combined_gas, aes(x = Date, y = PredVolatility, group = type, color=type))+ scale_x_date(date_labels  = "%Y",date_breaks = "1 years") + 
  theme_light() + theme(legend.position = c(0.1, 0.2))+ xlab(NULL)+ylab(NULL) + labs(color = "Model")

model_com_gas


#VaR 
quant <- quantile(oil$ReturnRate, 0.05)

dist_quan_oil <- qplot(oil$ReturnRate , geom = 'histogram') + geom_histogram(fill = 'grey' , bins = 30) +
  geom_histogram(aes(oil$ReturnRate[oil$ReturnRate < quantile(oil$ReturnRate , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Brent crude oil') +  theme_light()
dist_quan_oil
dist_quan_gas <- qplot(gas$ReturnRate , geom = 'histogram') + geom_histogram(fill = 'grey' , bins = 30) +
  geom_histogram(aes(gas$ReturnRate[gas$ReturnRate < quantile(gas$ReturnRate , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Natural gas') +  theme_light()

combined_plot_dist <- grid.arrange(dist_quan_oil,dist_quan_gas, ncol = 2)


# fitting 
quantile(gas$ReturnRate, 0.05)
fitdist(distribution = 'std' , x = oil$ReturnRate)$pars
shape_oil <- 3.0443696630


qplot(gas$ReturnRate , geom = 'histogram') + geom_histogram(fill = 'grey' , bins = 30) +
  geom_histogram(aes(gas$ReturnRate[final_gas$ReturnRate < quantile(gas$ReturnRate , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Daily Returns') + theme_light()

garchfitN_oil <- ugarchfit(data = oil$ReturnRate, spec = garchspecN)
com_oil <- qplot(y = oil$ReturnRate , x =1:3387 , geom = 'point') + geom_point(colour = 'lightgrey' , size = 2) + geom_line(aes(y = garchfitN_oil@fit$sigma*(-1.485151) , x = 1:3387), color = "red") + 
   geom_hline(yintercept = sd(oil$ReturnRate)*qnorm(0.05) , colour = 'black' , size = 0.7) + theme_light() + labs(x = "Brent crude oil" , y = 'Daily Returns') 
com_oil

garchfitGJR_gas <- ugarchfit(data = gas$ReturnRate, spec = garchspecGJR)
com_gas <- qplot(y = gas$ReturnRate , x =1:3413 , geom = 'point') + geom_point(colour = 'lightgrey' , size = 2) + geom_line(aes(y = garchfitGJR_gas@fit$sigma*(-1.485151) , x = 1:3413), color = "red") + 
  geom_hline(yintercept = sd(gas$ReturnRate)*qnorm(0.05) , colour = 'black' , size = 0.7) + theme_light() + labs(x = "Natural gas", y = "") 
com_gas

combined_comp <- grid.arrange(com_oil,com_gas, ncol = 2)


#VaR forecasting (OIL)
length(VaR95_td)

fitdist(distribution = 'std' , x = oil$ReturnRate)$pars
shape_gas <- 3.0443696630  
model.roll <- ugarchroll(spec = garchspecN, data = oil$ReturnRate , n.start = 2887 , refit.every = 50 ,
                        refit.window = 'moving')

VaR95_td = mean(oil$ReturnRate) + model.roll@forecast$density[,'Sigma']*qdist(distribution='std', shape=shape_oil, p=0.05)


obs_set_oil <- qplot(y = VaR95_td , x = 1:500 , geom = 'line') +
  geom_point(aes(x = 1:500 , y = oil$ReturnRate[2888:3387] , color = as.factor(oil$ReturnRate[2888:3387] < VaR95_td)) , size = 2) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set observation (Brent crude oil)') + theme_light() + 
  theme(legend.position = 'none')
obs_set_oil


#VaR forecasting (GAS)

fitdist(distribution = 'std' , x = gas$ReturnRate)$pars
shape_gas <- 4.0277888728 

model.roll2 <- ugarchroll(spec = garchspecGJR, data = gas$ReturnRate , n.start = 2913 , refit.every = 50 ,
                         refit.window = 'moving')

VaR95_td2 = mean(gas$ReturnRate) + model.roll2@forecast$density[,'Sigma']*qdist(distribution='std', shape=shape_gas, p=0.05)

obs_set_gas <- qplot(y = VaR95_td2 , x = 1:500 , geom = 'line') +
  geom_point(aes(x = 1:500 , y = gas$ReturnRate[2914:3413] , color = as.factor(gas$ReturnRate[2914:3413] < VaR95_td2)) , size = 2) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = '' , x = 'Test set observation (natural gas)') + theme_light() + 
  theme(legend.position = 'none')

combined_obs <- grid.arrange(obs_set_oil,obs_set_gas, ncol = 2)