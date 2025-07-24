rm(list = ls())

#============================ Install Packages ============================
library(TSA)
library(lmtest)
library(tseries)
library(forecast)
library(readr)
library(stats)

#============================ Set working directory & Read file ============================
# Set working directory
setwd("D:/Phạm Võ Vĩnh Phương/RMIT University/Assessments/Sem 3/Time Series Analysis/Asm 2")

# Read dataset
bitcoin <- read_csv("Bitcoin_dataset.csv")

#============================ Converting to time series & plot ============================
# Convert to time series object
bitcoin_ts <- ts(bitcoin$Bitcoin, start = c(2011, 8), end = c(2025, 1), frequency = 12)
bitcoin_ts

#Fit linear regression model
t <- time(bitcoin_ts)
t
linear_bitcoin <- lm(bitcoin_ts ~ t)

# Plot the time series
plot(bitcoin_ts, 
     ylab = "Deviation Index",
     xlab = "Time",
     type = 'o',
     main = "Time Series Plot for Bitcoin Index")
abline(linear_bitcoin, col = "#7e0000")

# Create and Plot The First Lag of the Bitcoin Series
y = bitcoin_ts
x = zlag(bitcoin_ts)
index = 2:length(x) 
cor(y[index],x[index])

plot(y=bitcoin_ts,x=zlag(bitcoin_ts),
     ylab='Index',
     xlab='Time',
     main= "Scatter plot of Bitcoin Index Year-to-Month")

# Descriptive
summary(bitcoin_ts)

#============================ Stationarity & normality check for time series object ============================
# ACF & PACF
par(mfrow=c(1,2))
acf(bitcoin_ts, main = "ACF plot of Bitcoin TS")
pacf(bitcoin_ts, main = "PACF plot of Bitcoin TS")
par(mfrow=c(1,1))

# Augmented Dickey-Fuller Test
adf.test(bitcoin_ts)

# Q-Q PLot
qqnorm(y=bitcoin_ts, main = "QQ plot of Bitcoin TS")
qqline(y=bitcoin_ts, col = 2, lwd = 1, lty = 2)

# Shapiro-Wilk Test
shapiro.test(bitcoin_ts)

#============================ Transformation ============================
# Box-cox Transformation
BC <- BoxCox.ar(bitcoin_ts, lambda = seq(0, 1, 0.01))
BC$ci
lambda <- BC$lambda[which(max(BC$loglike) == BC$loglike)]
lambda
bitcoin_bc = ((bitcoin_ts^lambda)-1)/lambda

par(mfrow=c(1,1))
plot(bitcoin_bc,
     type='o', 
     ylab ="INdex", 
     main="Time series plot for BC transformed Bitcoin TS")

# Log Transformation
bitcoin_log <- log(bitcoin_ts)
plot(bitcoin_log, 
     ylab = "Index",
     xlab = "Time",
     main = "Time Series Plot for Log transformed Bitcoin TS",
     type = "o")


  # Q-Q PLot
qqnorm(y=bitcoin_bc, main = "QQ plot for BC transformed Bitcoin TS")
qqline(y=bitcoin_bc, col = 2, lwd = 1, lty = 2)

  # Shapiro-Wilk Test
shapiro.test(bitcoin_bc)

#============================ Differencing ============================
# First difference 
  # Plot
bitcoin_dif <- diff(bitcoin_ts, differences = 1)
plot(bitcoin_dif,
     ylab = "Index",
     xlab = "Time",
     main = "BC transformed and first differenced Bitcoin TS",
     type = "o")

  # ACF & PACF
par(mfrow=c(1,2))
acf(bitcoin_dif, main = "ACF plot for the First Differenced Bitcoin TS")
pacf(bitcoin_dif, main = "PACF plot for the First Differenced Bitcoin TS")
par(mfrow=c(1,1))

  # Augmented Dickey-Fuller Test
adf.test(bitcoin_dif)

  # Phillips-Perron Unit Root Test
pp.test(bitcoin_dif)

  # KPSS Test
kpss.test(bitcoin_dif)

#============================ Propose possible models using specification tools ============================
# ACF & PACF
par(mfrow=c(1,2))
acf(bitcoin_dif, main = "ACF plot for the First Differenced Bitcoin TS")
pacf(bitcoin_dif, main = "PACF plot for the First Differenced Bitcoin TS")
par(mfrow=c(1,1))
#-- {ARIMA(3,1,2)}

# EACF
eacf(bitcoin_dif, ar.max = 5, ma.max = 5)
#-- {ARIMA(0,1,1), ARIMA(1,1,0), ARIMA(1,1,1)}

# BIC Table
resBit = armasubsets(y = bitcoin_dif,
                     nar = 14, nma = 14,
                     y.name = 'p',
                     ar.method = 'ols')
plot(resBit)
#-- {ARIMA(8,1,11), ARIMA(8,1,15)}

#-- Possible models: {ARIMA(3,1,2), ARIMA(0,1,1), ARIMA(1,1,0), ARIMA(1,1,1), ARIMA(8,1,11)}

#============================ Fit the models ============================
# ARIMA(3,1,2)
model.312 = arima(bitcoin_bc, order = c(3,1,2), method = 'ML')
coeftest(model.312)

model.312CSS = arima(bitcoin_bc, order = c(3,1,2), method = 'CSS')
coeftest(model.312CSS)

model.312CSSML = arima(bitcoin_bc, order = c(3,1,2), method = 'CSS-ML')
coeftest(model.312CSSML)


# ARIMA(0,1,1)
model.011 = arima(bitcoin_bc, order = c(0,1,1), method = 'ML')
coeftest(model.011)

model.011CSS = arima(bitcoin_bc, order = c(0,1,1), method = 'CSS')
coeftest(model.011CSS)

model.011CSSML = arima(bitcoin_bc, order = c(0,1,1), method = 'CSS-ML')
coeftest(model.011CSSML)


# ARIMA(1,1,0)
model.110 = arima(bitcoin_bc, order = c(1,1,0), method = 'ML')
coeftest(model.110)

model.110CSS = arima(bitcoin_bc, order = c(1,1,0), method = 'CSS')
coeftest(model.110CSS)

model.110CSSML = arima(bitcoin_bc, order = c(1,1,0), method = 'CSS-ML')
coeftest(model.110CSSML)


# ARIMA(1,1,1)
model.111 = arima(bitcoin_bc, order = c(1,1,1), method = 'ML')
coeftest(model.111)

model.111CSS = arima(bitcoin_bc, order = c(1,1,1), method = 'CSS')
coeftest(model.111CSS)

model.111CSSML = arima(bitcoin_bc, order = c(1,1,1), method = 'CSS-ML')
coeftest(model.111CSSML)


# ARIMA(8,1,11)
model.8111 = arima(bitcoin_bc, order = c(8,1,11), method = 'ML')
coeftest(model.8111)

model.8111CSS = arima(bitcoin_bc, order = c(8,1,11), method = 'CSS')
coeftest(model.8111CSS)

model.8111CSSML = arima(bitcoin_bc, order = c(8,1,11), method = 'CSS-ML')
coeftest(model.8111CSSML)


# Fit all models
aic_table <- AIC(model.312, model.011, model.110, model.111, model.8111)
aic_table[order(aic_table$AIC), ]

bic_table <- BIC(model.312, model.011, model.110, model.111, model.8111)
bic_table[order(bic_table$BIC), ]

# Use forecast package to get error measures
model.312A = Arima(bitcoin_bc,order=c(3,1,2),method='ML')
model.011A = Arima(bitcoin_bc,order=c(0,1,1),method='ML')
model.110A = Arima(bitcoin_bc,order=c(1,1,0),method='ML')
model.111A = Arima(bitcoin_bc,order=c(1,1,1),method='ML')
model.8111A = Arima(bitcoin_bc,order=c(8,1,11),method='ML')

Smodel.312A <- accuracy(model.312A)[1:7]
Smodel.011A <- accuracy(model.011A)[1:7]
Smodel.110A <- accuracy(model.110A)[1:7]
Smodel.111A <- accuracy(model.111A)[1:7]
Smodel.8111A <- accuracy(model.8111A)[1:7]

df.Smodels <- data.frame(
  rbind(Smodel.312A,Smodel.011A,Smodel.110A,
        Smodel.111A,Smodel.8111A)
)
colnames(df.Smodels) <- c("ME", "RMSE", "MAE", "MPE", "MAPE",
                          "MASE", "ACF1")
rownames(df.Smodels) <- c("ARIMA(3,1,2)", "ARIMA(0,1,1)", "ARIMA(1,1,0)",
                          "ARIMA(1,1,1)", "ARIMA(8,1,11)")

df.Smodels
