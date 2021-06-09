###ROBERTO REVIRIEGO MARTIN###
#TRABAJO FIN DE GRADO EN ECONOMÍA
#UNIVERSIDAD CARLOS III DE MADRID
#rreviriego.inbox@gmail.com

#LAS CRIPTOMONEDAS COMO COBERTURA A LAS POLÍTICAS DE EXPANSIÓN CUANTITATIVA:
#ANÁLISIS CAUSAL CON APROXIMACIÓN VAR

setwd("C:\\Users\\Roberto\\Dropbox\\TFG")

library(tidyverse)
library(tseries)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(vars)
library(urca)
library(xts)
library(TSstudio)
library(tsDyn)

data <- read.csv("datared.csv")
sink("./outputred.txt", append = T)

#crear ts
btc <- ts(data$BTC, start = c(2014,44), frequency = 52)
fed <- ts(data$FED, start = c(2014,44), frequency = 52)
rates <- ts(data$Rates, start = c(2014,44), frequency = 52)

#transformar logs
lbtc <- log(btc)
lfed <- log(fed)
lrates <- log(rates)


######ESTACIONARIEDAD --> ADF (unit root test)
######Visualmente se ve que no son estacionarias
adf.test(lbtc) #no estacionaria 
adf.test(lfed) #no estacionaria
adf.test(lrates) #no estacionaria

##primeras diferencias
dlbtc <- diff(lbtc)
dlfed <- diff(lfed)
dlrates <- diff(lrates)

adf.test(dlbtc) #estacionaria I(1)
adf.test(dlfed) #estacionaria I(1)
adf.test(dlrates) #estacionaria I(1)

#####MODELO 2.1 BTC, FED #######################################################

data2F <- cbind(lbtc,lfed)
colnames(data2F) <- cbind("BTC", "FED")

d_data2F <- cbind(dlbtc, dlfed)
colnames(d_data2F) <- cbind("dBTC", "dFED")


#######OPTIMAL LAG LENGTH
lag <- VARselect(data2F)
print(lag) #2 LAGS 


######COINTEGRACIÓN --> JOHANSEN 
coint2F <- ca.jo(data2F, type = "trace" , ecdet = "const", K = 10)
summary(coint2F)
#no existe cointegración --> VAR OK (con diferencias)



#######OPTIMAL LAG LENGTH (en diferencias)
lagd2F <- VARselect(d_data2F)
print(lagd2F) #1 LAG


######VAR
var2F <- VAR(d_data2F, p = 1, type = "const", season = NULL)
summary(var2F)


######STABILITY
roots_var2F <- roots(var2F, modulus = TRUE)
print(roots_var2F) #raices menores a 1 --> el sistema es estable: existe el numero correcto
#de lags.


#granger

grangerBTC2f <- causality(var2F, cause = "dBTC")
grangerBTC2f

grangerFED2f <- causality(var2F, cause = "dFED")
grangerFED2f

#impulse response

res_fed <- irf(var2F, impulse = "dFED", response = "dBTC", n.ahead = 26, boot = TRUE)
plot(res_fed, ylab = "dBTC", main = "Shock FED")

#descomposición varianza

vd2F <- fevd(var2F, n.ahead = 12)
plot(vd2F)

#########MODELO 2.2 BTC RATES ##################################################

data2R <- cbind(lbtc,lrates)
colnames(data2R) <- cbind("BTC", "RATES")

d_data2R <- cbind(dlbtc, dlrates)
colnames(d_data2R) <- cbind("dBTC", "dRATES")

data2 <- cbind(dlbtc, dlfed, dlrates)
colnames(data2) <- cbind("dBTC", "dFED", "dRATES")


#######OPTIMAL LAG LENGTH
lag <- VARselect(data2R)
print(lag) #3 LAGS 


######COINTEGRACIÓN --> JOHANSEN 
coint2R <- ca.jo(data2R, type = "trace" , ecdet = "const", K = 7)
summary(coint2R)
#no existe cointegración --> VAR OK (con diferencias)


#######OPTIMAL LAG LENGTH (en diferencias)
lagd2R <- VARselect(d_data2R)
print(lagd2R) #2 LAG

######VAR
var2R <- VAR(d_data2R, p = 2, type = "const", season = NULL)
summary(var2R)

######STABILITY
roots_var2R <- roots(var2R, modulus = TRUE)
print(roots_var2R)


#granger

grangerBTC2r <- causality(var2R, cause = "dBTC")
grangerBTC2r

grangerRATES2r <- causality(var2R, cause = "dRATES")
grangerRATES2r


#impulse response

res_rates <- irf(var2R, impulse = "dRATES", response = "dBTC", n.ahead = 26, boot = TRUE)
plot(res_rates, ylab = "dBTC", main = "Shock RATES")

#descomposición varianza

vd2R <- fevd(var2R, n.ahead = 12)
plot(vd2R)

######SERIAL
serial2R <- serial.test(var2R, lags.pt = 12, type = "PT.asymptotic")
serial2R


#structural 
stability2F <- stability(var2R, type = "OLS-CUSUM")
plot(stability2F)



#ECM
model2 <- VECM(data2R, 7, r=1, estim="2OLS")
summary(model2)

#coeff ect1 --> coeficiente de convergencia al equilibrio --> tarda mucho tiempo en
#converger al equilibrio

sink()
