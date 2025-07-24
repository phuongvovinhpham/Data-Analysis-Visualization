rm(list=ls())
library(TSA)
library(tidyr)
library(tseries)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(lmtest)
library(forecast)
library(fUnitRoots)
library(magrittr)
library(lubridate)
#library(FitAR)
#install.packages("https://cran.r-project.org/src/contrib/Archive/FitAR/FitAR_2.1-5.tar.gz", repos = NULL, type = "source")


sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  seasonal_acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

#=========seasonal function========
##################################################
# These functions are developed by               #  
# MATH1318 students                              #
# Le Van Tra Tran and Tin Trung Pham             #
# in 2024. WE thank them for their contribution! #
##################################################

# Helper function ---------------------------------------------------------------------

helper <- function(class = c("acf", "pacf"), ...) {
  
  # Capture additional arguments
  params <- match.call(expand.dots = TRUE)
  params <- as.list(params)[-1]
  
  # Calculate ACF/PACF values
  if (class == "acf") {
    acf_values <- do.call(acf, c(params, list(plot = FALSE)))
  } else if (class == "pacf") {
    acf_values <- do.call(pacf, c(params, list(plot = FALSE)))
  }
  
  # Extract values and lags
  acf_data <- data.frame(
    Lag = as.numeric(acf_values$lag),  
    ACF = as.numeric(acf_values$acf)   
  )
  
  # Identify seasonal lags to be highlighted
  seasonal_lags <- acf_data$Lag %% 1 == 0
  
  # Plot ACF/PACF values
  if (class == "acf") {
    do.call(acf, c(params, list(plot = TRUE)))
  } else if (class == "pacf") {
    do.call(pacf, c(params, list(plot = TRUE)))
  }
  
  # Add colored segments for seasonal lags
  for (i in which(seasonal_lags)) {
    segments(x0 = acf_data$Lag[i], y0 = 0, x1 = acf_data$Lag[i], y1 = acf_data$ACF[i], col = "red")
  }
}


# seasonal_acf ------------------------------------------------------------

seasonal_acf <- function(...) {
  helper(class = "acf", ...)
}


# seasonal_pacf -----------------------------------------------------------

seasonal_pacf <- function(...) {
  helper(class = "pacf", ...)
}


#----------data preprosessing-----------------

setwd("D:/Phạm Võ Vĩnh Phương/RMIT University/Assessments/Sem 3/Time Series Analysis/Asm 3")

energy <- read.csv("energy_dataset.csv")

energy <- energy %>% select(time, total.load.actual)

energy %<>% mutate(
  time = ymd_hms(energy$time, tz = "Europe/Paris")
)

energy %<>% mutate(
  day = day(energy$time),
  week = week(energy$time),
  month = month(energy$time),
  year = year(energy$time)
)

energy2 <- energy %>%
  group_by(week, year)  %>%
  summarise(mean_load = mean(total.load.actual, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, week)

energy_ts <- ts(energy2$mean_load, start = c(2015,1), frequency = 53)

end(energy_ts)
energy_ts


plot(energy_ts, type = "o",
     ylab = "Total Actual Load",
     main='Time series plot of Actual Energy Consumed')

shapiro.test(energy_ts)

#seasonal pacf and acf
par(mfrow=c(1,2))
seasonal_acf(energy_ts, lag.max = 215)
seasonal_pacf(energy_ts, lag.max = 215)
par(mfrow=c(1,1))

# Seasonality and existence of trend are obvious from the ACF and PACF plots

adf.test(energy_ts)
pp.test(energy_ts)
kpss.test(energy_ts)


#boxcox
BC <- BoxCox.ar(energy_ts) #,lambda = seq(-1, 0.5, 0.01) If you get an error.
BC$ci
lambda <- BC$lambda[which(max(BC$loglike) == BC$loglike)]
lambda 

energy_ts_BC = ((energy_ts^lambda)-1)/lambda

par(mfrow=c(1,2))
plot(energy_ts, type = "o")
plot(energy_ts_BC, type = "o")
par(mfrow=c(1,1))

#=========================================modelling=============================
#=======================model 1======================
m.energy_SARIMA000_010 = Arima(energy_ts,
                   order=c(0,0,0),
                   seasonal=list(order=c(0,1,0),
                                 period=53))
res.m.energy_SARIMA000_010 <- residuals(m.energy_SARIMA000_010)

par(mfrow=c(1,1))
plot(res.m.energy_SARIMA000_010)


par(mfrow=c(1,2))
seasonal_acf(res.m.energy_SARIMA000_010, lag.max = 215)
seasonal_pacf(res.m.energy_SARIMA000_010, lag.max = 215)
par(mfrow=c(1,1))


#=======================model 2======================
m.energy_SARIMA000_011 = Arima(energy_ts,
                               order=c(0,0,0),
                               seasonal=list(order=c(0,1,1),
                                             period=53))
res.m.energy_SARIMA000_011 <- residuals(m.energy_SARIMA000_011)
par(mfrow=c(1,1))
plot(res.m.energy_SARIMA000_011)



par(mfrow=c(1,2))
seasonal_acf(res.m.energy_SARIMA000_011, lag.max = 215)
seasonal_pacf(res.m.energy_SARIMA000_011, lag.max = 215)
par(mfrow=c(1,1))

eacf(res.m.energy_SARIMA000_011)

resBIC = armasubsets(y=res.m.energy_SARIMA000_011,
                     nar=7,
                     nma=7,y.name='p',ar.method='ols')
plot(resBIC)

#====================possible models===========
P <- c(8,7,0,1,0,1,0,4,7) 
D <- rep(0,length(P))
Q <- c(8,7,4,4,5,5,7,4,4)


seasonal_P <- rep(0,length(P))
seasonal_D <- rep(1,length(D))
seasonal_Q <- rep(1,length(Q))


# Create empty lists to store models
models_css <- list()
models_ml <- list()
models_CSSML <- list()

# Loop through all combinations of P, D, Q
for (i in seq_along(P)) {
  
  #model name
  model_name <- paste0("SARIMA(", P[i], ",", D[i], ",", Q[i],
                       ")(0,1,1)53")
  
  # Build and store models using CSS and ML methods
  models_css[[model_name]] <- Arima(energy_ts,
                                    order = c(P[i], D[i], Q[i]),
                                    seasonal=list(order=c(0,1,1),
                                                  period=53),
                                    method = "CSS")
  models_ml[[model_name]] <- Arima(energy_ts,
                                   order = c(P[i], D[i], Q[i]),
                                   seasonal=list(order=c(0,1,1),
                                                 period=53),
                                   method = "ML")
  models_CSSML[[model_name]] <- Arima(energy_ts,
                                      order = c(P[i], D[i], Q[i]),
                                      seasonal=list(order=c(0,1,1),
                                                    period=53),
                                      method = "CSS-ML")
  
}




#Save models
save(file='SARIMA01153CSS.Rdata', models_css)
save(file='SARIMA01153ML.Rdata', models_ml)
save(file='SARIMA01153CSSML.Rdata', models_CSSML)
load(file='SARIMA01153CSS.Rdata')
load(file='SARIMA01153ML.Rdata')
load(file='SARIMA01153CSSML.Rdata')

# Loop over all model names and extract coefficients
for (name in names(models_css)) {
  cat("\n---", name, "(CSS) ---\n")
  print(coeftest(models_css[[name]]))
  
  cat("\n---", name, "(ML) ---\n")
  print(coeftest(models_ml[[name]]))
  
  cat("\n---", name, "(CSS-ML) ---\n")
  print(coeftest(models_CSSML[[name]]))
}


#sort AIC and BIC score
ML_names <- names(models_ml)

sort.score(AIC(models_ml[[ML_names[1]]],
               models_ml[[ML_names[2]]],
               models_ml[[ML_names[3]]],
               models_ml[[ML_names[4]]],
               models_ml[[ML_names[5]]],
               models_ml[[ML_names[6]]],
               models_ml[[ML_names[7]]],
               models_CSSML[[ML_names[1]]],
               models_CSSML[[ML_names[2]]],
               models_CSSML[[ML_names[3]]],
               models_CSSML[[ML_names[4]]],
               models_CSSML[[ML_names[5]]],
               models_CSSML[[ML_names[6]]],
               models_CSSML[[ML_names[7]]],
               models_CSSML[[ML_names[8]]],
               models_CSSML[[ML_names[9]]]), score = "aic")

sort.score(BIC(models_ml[[ML_names[1]]],
               models_ml[[ML_names[2]]],
               models_ml[[ML_names[3]]],
               models_ml[[ML_names[4]]],
               models_ml[[ML_names[5]]],
               models_ml[[ML_names[6]]],
               models_ml[[ML_names[7]]],
               models_CSSML[[ML_names[1]]],
               models_CSSML[[ML_names[2]]],
               models_CSSML[[ML_names[3]]],
               models_CSSML[[ML_names[4]]],
               models_CSSML[[ML_names[5]]],
               models_CSSML[[ML_names[6]]],
               models_CSSML[[ML_names[7]]],
               models_CSSML[[ML_names[8]]],
               models_CSSML[[ML_names[9]]]), score = "bic")



AIC(models_ml[[ML_names[5]]])



# MLmodels <- list()
# CSSmodels <- list()
# 
# for (name in names(models_ml)) {
#   MLmodels[[name]] <- accuracy(models_ml[[name]])[1:9]
# }
# 
# df.MLmodels <- data.frame(do.call(rbind,MLmodels))
# colnames(df.MLmodels) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", 
#                           "MASE", "ACF1")
# 
# rownames(df.MLmodels) <- c("SARIMA(8,0,8)(0,1,1)53_ML",
#                           "SARIMA(7,0,7)(0,1,1)53_ML",
#                           "SARIMA(0,0,4)(0,1,1)53_ML",
#                           "SARIMA(1,0,4)(0,1,1)53_ML",
#                           "SARIMA(0,0,5)(0,1,1)53_ML",
#                           "SARIMA(1,0,5)(0,1,1)53_ML",
#                           "SARIMA(0,0,7)(0,1,1)53_ML",
#                           "SARIMA(4,0,4)(0,1,1)53_ML",
#                           "SARIMA(7,0,4)(0,1,1)53_ML")
# 
# MLmodels <- round(df.MLmodels, digits = 4)
# 
# for (name in names(models_css)) {
#   CSSmodels[[name]] <- accuracy(models_css[[name]])[1:7]
# }
# 
# df.CSSmodels <- data.frame(do.call(rbind,CSSmodels))
# colnames(df.CSSmodels) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", 
#                            "MASE", "ACF1")
# 
# rownames(df.CSSmodels) <- c("SARIMA(8,0,8)(0,1,1)53_CSS",
#                             "SARIMA(7,0,7)(0,1,1)53_CSS",
#                             "SARIMA(0,0,4)(0,1,1)53_CSS",
#                             "SARIMA(1,0,4)(0,1,1)53_CSS",
#                             "SARIMA(0,0,5)(0,1,1)53_CSS",
#                             "SARIMA(1,0,5)(0,1,1)53_CSS",
#                             "SARIMA(0,0,7)(0,1,1)53_CSS",
#                             "SARIMA(4,0,4)(0,1,1)53_CSS",
#                             "SARIMA(7,0,4)(0,1,1)53_CSS")
# 
# CSSmodels <- round(df.CSSmodels, digits = 4)
# 
# Error_measures <- rbind(MLmodels,CSSmodels)
# 
# 
# m.energy_SARIMA004_011 = Arima(energy_ts,
#                                order=c(6,0,7),
#                                seasonal=list(order=c(0,1,1),
#                                              period=53),
#                                method = "CSS-ML")
# 
# 
# coeftest(m.energy_SARIMA004_011)
# 
# residual.analysis(model = m.energy_SARIMA004_011)
# 




#other error measures
names(models_ml) <- paste0(names(models_ml), "_ML")
names(models_css) <- paste0(names(models_ml), "_CSS")
names(models_CSSML) <- paste0(names(models_ml), "_CSSML")


Allmodels <- list()
fitted_models <- c(models_ml, models_css, models_CSSML)

for (name in names(fitted_models)) {
  Allmodels[[name]] <- accuracy(fitted_models[[name]])[1:7]
}

df.Allmodels <- data.frame(do.call(rbind,Allmodels))
colnames(df.Allmodels) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", 
                           "MASE", "ACF1")


rownames(df.Allmodels) <- c("SARIMA(8,0,8)(0,1,1)53_ML",
                            "SARIMA(7,0,7)(0,1,1)53_ML",
                            "SARIMA(0,0,4)(0,1,1)53_ML",
                            "SARIMA(1,0,4)(0,1,1)53_ML",
                            "SARIMA(0,0,5)(0,1,1)53_ML",
                            "SARIMA(1,0,5)(0,1,1)53_ML",
                            "SARIMA(0,0,7)(0,1,1)53_ML",
                            "SARIMA(4,0,4)(0,1,1)53_ML",
                            "SARIMA(7,0,4)(0,1,1)53_ML",
                            "SARIMA(8,0,8)(0,1,1)53_CSS",
                            "SARIMA(7,0,7)(0,1,1)53_CSS",
                            "SARIMA(0,0,4)(0,1,1)53_CSS",
                            "SARIMA(1,0,4)(0,1,1)53_CSS",
                            "SARIMA(0,0,5)(0,1,1)53_CSS",
                            "SARIMA(1,0,5)(0,1,1)53_CSS",
                            "SARIMA(0,0,7)(0,1,1)53_CSS",
                            "SARIMA(4,0,4)(0,1,1)53_CSS",
                            "SARIMA(7,0,4)(0,1,1)53_CSS",
                            "SARIMA(8,0,8)(0,1,1)53_CSSML",
                            "SARIMA(7,0,7)(0,1,1)53_CSSML",
                            "SARIMA(0,0,4)(0,1,1)53_CSSML",
                            "SARIMA(1,0,4)(0,1,1)53_CSSML",
                            "SARIMA(0,0,5)(0,1,1)53_CSSML",
                            "SARIMA(1,0,5)(0,1,1)53_CSSML",
                            "SARIMA(0,0,7)(0,1,1)53_CSSML",
                            "SARIMA(4,0,4)(0,1,1)53_CSSML",
                            "SARIMA(7,0,4)(0,1,1)53_CSSML")



df.Allmodels


#Residual Analysis
#residual.analysis(model = models_ml[[1]])

#future measure
future = forecast(models_ml[[1]], h = 10)
future
plot(future)




#=======================others==========================

m.energy_SARIMA000_101 = Arima(energy_ts,
                               order=c(0,1,0),
                               seasonal=list(order=c(1,0,1),
                                             period=53))
res.m.energy_SARIMA000_101 <- residuals(m.energy_SARIMA000_101)
par(mfrow=c(1,1))
plot(res.m.energy_SARIMA000_101)


par(mfrow=c(1,2))
seasonal_acf(res.m.energy_SARIMA000_101, lag.max = 215)
seasonal_pacf(res.m.energy_SARIMA000_101, lag.max = 215)
par(mfrow=c(1,1))

eacf(res.m.energy_SARIMA000_011)

resBIC = armasubsets(y=res.m.energy_SARIMA000_011,
                     nar=8,
                     nma=8,y.name='p',ar.method='ols')
plot(resBIC)




models_SARIMA101 <- Arima(energy_ts,
                                    order = c(0, 1, 4),
                                    seasonal=list(order=c(1,0,1),
                                                  period=53),
                                    method = "ML")
coeftest(models_SARIMA101)
residual.analysis(models_SARIMA101)
#futur measure
future = forecast(models_SARIMA101, h = 10)
future
plot(future)
