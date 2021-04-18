# Stress test using Colombian Data
# Duvan Mejia Cortes
# Universidad Surcolombiana.


library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)

#Load the Dataset
okun <- read.csv(file.choose())

# A simple graph
ggplot(data = okun) + geom_point(mapping = aes(x = unem, y = real_gdp_growth))

#Declare our time Series Variables
gdp  <- ts(okun$real_gdp_growth, start = c(1999,3), frequency = 4)
desempleo <- ts(okun$unem, start = c(1999,3), frequency = 4)

#Plot the series
autoplot(cbind(gdp,unem))

#OLS
OLS1 <- lm(gdp ~ unem)
summary(OLS1)

#Determine the Persistence of the model
acf(gdp,main = "ACF for real GDP Growth")
pacf(gdp,main = "PACF for real GDP Growth")

acf(unem,main = "ACF for Unemployment")
pacf(unem,main = "PACF for Unemployment")


#Finding the Optimal LAG
okun.bv <- cbind(gdp, unem)
colnames(okun.bv) <-cbind("GDP", "Desempleo")

lagselect <- VARselect(okun.bv, lag.max = 10, type = "const")
lagselect$selection

#Building VAR
ModelOkun1 <- VAR(okun.bv, p = 4, type = "const", season = NULL, exog = NULL)
summary(ModelOkun1)
