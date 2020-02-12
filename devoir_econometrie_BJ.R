# Donner le nom du répertoire de travail
setwd('K:/M2ST')

# Donner le nom du répertoire pour installer les librairies
.libPaths("K:/M2ST/Lib")

# Setup
library(httr)
library(curl)
library(astsa)
library(Quandl)
library(urca)
library(ggplot2)
library(forecast)
library(tswge)
library(TSA)


# variable crea

data(nporg)
rownames(nporg) <- nporg$year

employment <- nporg[6]
employment <- na.omit(employment)
employment$logemp <- ts(log(employment$emp), start = 1890, end = 1970)

### ### ### ### ### ### ### ### ###
### Caracteristiques des donnees ###
### ### ### ### ### ### ### ### ###

#Descriptive statistics
summary(nporg$emp) #30NA / 47700 mean /81820 Max / 21100 Min

# Plot time series
plot.ts(employment$logemp) #start in 1890
plot.ts(employment$emp) #start in 1890
 
# Getting the autocorrelations
acf2(employment$logemp, na.action = na.pass)
acf2(employment$emp,    na.action = na.pass)

# Tendance stochastique il faut différencier ,( il y a une racine unité il faut différencier)
# a voir si cette partie est nécessaire dans chacun des cas
demp <- diff(employment$logemp)
demp <- demp[-1]
acf2(demp2)



### ### ### ### ### ### ### ### ###
####        Methode de TT       ### 
### ### ### ### ### ### ### ### ###

arorder <- 3
TTmodel <- Arima(employment$logemp,order=c(arorder,0,0),method="CSS")
coef(TTmodel)

phi <- coef(TTmodel)[1:arorder]
sort(Mod(polyroot(c(1,-phi))))
factor.wge(phi) # on a une racine unité

# Test de racine unitaire
test.racine1 = ur.df(employment$logemp, type = "trend", lags =1) 
summary(test.racine1)
test.racine2 = ur.df(employment$logemp, type = "trend", lags =2) 
summary(test.racine2)
test.racine3 = ur.df(employment$logemp, type = "trend", lags =3) 
summary(test.racine3)


# EACF

eacf(demp)

# Estimation modele ARIMA et verification
mymodel <- Arima(employment$logemp,order=c(0,1,1), include.constant = TRUE) # FALSE/TRUE ?
summary(mymodel)
# order (p,d,q)
# Quand on a bien modélisé il ne reste que les Innovation et cest un bruit blanc 
# donc il n'y plus d'autocorrélation
u <- residuals(mymodel)
acf2(u)


# factor.wge(coef(mymodel)[0:4]) #AR
factor.wge(-coef(mymodel)[1]) #MA


#hypothese nulle bruit blanc
Box.test(u, lag=1, type="Ljung-Box", fitdf = 2)
# p-value très élevée on ne rejette pas H0
# donc l'hypothèse que c'est un bruit blanc n'est pas rejetté
# ce qui nous comforte dans notre bonne modélisation

aic5.wge(demp, type="bic")
aic5.wge(demp, type="aic")


# Simuler des donnees 
simdata <- arima.sim(n=length(demp), list(ma=c(coef(mymodel))), sd =sqrt(mymodel$sigma2))
plot.ts(simdata)
plot.ts(demp)
acf2(simdata)
acf2(demp)
fit <- forecast(mymodel)
plot(fit)



datain <- window(employment$logemp, start=1890, end=1945)
dataout <- window(employment$logemp, start=1946, end= 1960)
modelin <- Arima(datain,order=c(0,1,1), include.constant = TRUE)
plot(forecast(modelin,h=10))
lines(data)

modelout <- Arima(dataout,model=modelin)
accuracy(modelin)
accuracy(modelout)
accuracy(modelout, h=5)



























