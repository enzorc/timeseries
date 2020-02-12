##########################################################################################
##########################################################################################
##################### SETTING WD AND UPLOADING LIBRARIES
##########################################################################################
##########################################################################################

setwd('F:/avocado')
.libPaths("F:/Lib")


library(lmtest)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(astsa)
library(vars)
library(TSA)
library(urca)
library(forecast)
library(dynlm)
library(xtable)

##########################################################################################
##########################################################################################
##################### IMPORTING FILES AND FORMATTING
##########################################################################################
##########################################################################################

###### AIRQUALITY ###########################################################################

aqi15 <- read.csv("aqidaily2015.csv")
aqi15$SO2 <- as.numeric(as.character(aqi15$SO2))
aqi15[is.na(aqi15)] <- 0
aqi16 <- read.csv("aqidaily2016.csv")
aqi17 <- read.csv("aqidaily2017.csv")
aqi18 <- read.csv("aqidaily2018.csv")

total <- rbind(aqi15, aqi16)
total <- rbind(total, aqi17)
total <- rbind(total, aqi18)
total <- total[, -c(3:6)]
total$Date <- as.Date(total$Date, format = "%m/%d/%Y")


###### AVOCADO ###############################################################################

data <- read.csv("avocado.csv")
data$Date <- as.Date(data$Date, "%Y-%m-%d")

###### FULL (merge) ##########################################################################

df <- merge(x = data, y = total, by = "Date", all.x = TRUE)
df <- df[, -c(2)]

###### CLEANING AND FILTERING ################################################################

df <- df %>% filter(region == "LosAngeles")
df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]

###### FORMATTING AVOCADO ####################################################################

organic <- df %>% filter(type == "organic")
rownames(organic) <- organic$Date
ts_organic <- ts(na.omit(organic$AveragePrice), frequency = 52, start = c(2015,1) )
d_tso <- diff(ts_organic)
organic$dpriceo<-c(NA,diff(organic$AveragePrice))

conventional <- df %>% filter(type == "conventional")
rownames(conventional) <- conventional$Date
ts_conv <- ts(na.omit(conventional$AveragePrice), frequency = 52 , start = c(2015,1))
d_tsc <- diff(ts_conv)
conventional$dpricec<-c(NA,diff(conventional$AveragePrice))

###### FORMATTING AIRQUALITY #################################################################

ts_pm <- ts(na.omit(organic$PM25), frequency = 52, start = c(2015,1) , end = c(2018,13))
d_pm <- diff(ts_pm)
organic$dpm<-c(NA,diff(organic$PM25))

ts_oz <- ts(na.omit(organic$Ozone), frequency = 52 , start = c(2015,1) , end = c(2018,13))
d_oz <- diff(ts_oz)
organic$doz<-c(NA,diff(organic$Ozone))



##########################################################################################
##########################################################################################
##################### DESCRIPTIVE ANALYSIS
##########################################################################################
##########################################################################################

qplot(AveragePrice, data=df, geom="density", fill=type, alpha=I(.5),
      main="Distribution of Average Price", xlab="Average Price",
      ylab="Density")

qplot(type, AveragePrice, data=df, geom=c("boxplot", "jitter"),
      fill=type, main="Dispersion of the price for each type",
      xlab="Type", ylab="Average Price")

ggplot(df, aes(x = Date, y = AveragePrice, fill = type)) +   # Fill column
    geom_bar(stat = "identity") +   # draw the bars
    labs(title="difference in price between organic and conventional")

a <- ggplot(organic, aes(x = Date)) +   # Fill column
    geom_bar(stat = "identity", aes(y = AveragePrice), color="#00BFC4") +   # draw the bars
    labs(title="difference in price between organic and conventional")

b<- ggplot(conventional, aes(x = Date)) +   # Fill column
  geom_bar(stat = "identity", aes(y = AveragePrice), color="#F8706D") +   # draw the bars
    labs(title="difference in price between organic and conventional")

# prop_diff1 <- ggplot(df, aes(x = Date, y = AveragePrice, fill = type)) +   # Fill column
#   geom_bar(stat = "identity") +   # draw the bars
#   labs(title="difference in price between organic and conventional")

# prop_diff2 <- ggplot(df, aes(x = Date, y = Total.Volume, fill = type)) +   # Fill column
#   geom_bar(stat = "identity") +   # draw the bars
#   labs(title="difference in quantity between organic and conventional")

plot_grid(a, b, nrow=2)


##########################################################################################
##########################################################################################
##################### UNIVARIATE TIME SERIES ANALYSIS
##########################################################################################
##########################################################################################

##########################################################################################
### AVOCADO PRICES #######################################################################
##########################################################################################

###### TIME SERIES PLOT ##################################################################

# ggplot(data = df, aes(x = Date, y = AveragePrice, col=type)) +
#   geom_line() +
#   facet_grid(type ~ .)

ggplot(organic, aes(x=Date)) +
  geom_line(aes(y = AveragePrice), color="#00BFC4")

###### ACF ###############################################################################

## organic
organic_acf1 <- organic$AveragePrice %>% acf2()
organic_acf2 <- organic$AveragePrice %>% diff() %>% acf2()
# plot_grid(organic_acf1, organic_acf2, nrow=2)

## conventional
# conv_acf1 <- conventional$AveragePrice %>% acf2()
# conv_acf2 <- conventional$AveragePrice %>% diff %>% acf2()
# plot_grid(conv_acf1, conv_acf2, nrow=2)


###### TESTING FOR UNIT ROOT AND STATIONNARITY ###########################################


## organic
# adf.test(ts_organic)
# p>0.05, hypothesis can't be rejected
# ts_organic %>% diff() %>% adf.test()
# p<0.05
# Therefore, differencing is used to remove trends and seasonal patterns

test.racine1 = ur.df(ts_organic, type = "drift", lags =2)
summary(test.racine1)

test.racine1 = ur.df(d_tso, type = "drift", lags =1)
summary(test.racine1)

## conventional
# adf.test(ts_conv)
# p>0.05, hypothesis can't be rejected
# ts_conv %>% diff() %>% adf.test()
# p<0.05.
# Therefore, differencing is used to remove trends and seasonal patterns

# test.racine2 = ur.df(ts_conv, type = "drift", lags =1)
# summary(test.racine2)

###### DIFFERENTIATE TIME SERIES PLOT ####################################################

## organic
organic_ts2 <- ggplot(data = organic, aes(x = Date, y = dpriceo, colors=type)) +
  geom_line(color="#00BFC4")

## conventional
conv_ts2 <- ggplot(data = conventional, aes(x = Date, y = dpricec, colors=type)) +
  geom_line(color="#F8706D")

plot_grid(conv_ts2, organic_ts2, nrow=2)

###### DETERMINATION OF THE MODEL #########################################################

## ORDER (p,d,q)
eacf(d_tso)
# auto.arima(organic$AveragePrice, d=1, seasonal = TRUE)

## Testing model with AIC BIC criteria

mymodel <- Arima(ts_organic, order=c(0,1,2), include.constant = TRUE)
summary(mymodel)
mymodel <- Arima(organic$AveragePrice, order=c(1,1,2), include.constant = TRUE)#, seasonal = list(order=c(1,1,1), period = 52), method="CSS" )
summary(mymodel)
## checking residuals 
u <- residuals(mymodel)
acf2(u)

## testing residuals ( white noise - ljung box)
Box.test(u, lag=1, type="Ljung-Box") # fitdf = p+q AND lag > fitdf

###### FORECASTING #######################################################################

fit <- forecast(mymodel, h= 12)
plot(fit)

##########################################################################################
### AIRQUALITY OZONE #####################################################################
##########################################################################################

###### TIME SERIES PLOT ##################################################################

# pm_ts1 <- ggplot(organic, aes(x=Date)) +
#   geom_line(aes(y = PM25), color = "#00BFC4")
# 
# ozone_ts1 <- ggplot(organic, aes(x=Date)) +
#   geom_line(aes(y = Ozone), color="#F8706D")
# 
# plot_grid(pm_ts1, ozone_ts1, nrow=2)

ggplot(organic, aes(x=Date)) +
  geom_line(aes(y = Ozone), color="#00BFC4")

###### ACF ################################################################################

## pm25
# acfpm25 <- organic$PM25 %>% acf2()
# dacfpm25 <- organic$PM25 %>% diff() %>% acf2()
# plot_grid(acfpm25, dacfpm25, nrow=2)

## Ozone
acfozone <- organic$Ozone %>% acf2()
dacfozone <- organic$Ozone %>% diff() %>% acf2()
# plot_grid(acfozone, dacfozone, nrow=2)

###### TESTING FOR UNIT ROOT AND STATIONNARITY ############################################

## Ozone
adf.test(organic$Ozone)
kpss.test(organic$Ozone)
# As p>0.05, hypothesis cannot be rejected.
# ts_oz %>% diff() %>% adf.test()
# As p<0.05.
# Therefore, differencing is used to remove trends and seasonal patterns

test.racine2 = ur.df(ts_oz, type = "drift", lags =5)
summary(test.racine2)
test.racine2 = ur.df(d_oz, type = "drift", lags =3)
summary(test.racine2)

## pm25
# adf.test(ts_pm)
# As p>0.05, hypothesis cannot be rejected.
# ts_pm %>% diff() %>% adf.test()
# As p<0.05.
# Therefore, differencing is used to remove trends and seasonal patterns
# test.racine1 = ur.df(ts_pm, type = "drift", lags =2)
# summary(test.racine1)

###### DIFFERENTIATE TIME SERIES PLOT ####################################################

## pm25
# pm_ts2 <- ggplot(data = organic, aes(x = Date, y = dpm)) +
#   geom_line(color="#00BFC4")

## Ozone
# ozone_ts2 <- ggplot(data = organic, aes(x = Date, y = doz)) +
#   geom_line(color="#F8706D")

# plot_grid(pm_ts2, ozone_ts2, nrow=2)

ggplot(data = organic, aes(x = Date, y = doz)) +
    geom_line(color="#F8706D")

###### DETERMINATION OF THE MODEL ########################################################

## ORDER (p,d,q)
eacf(d_oz)

## Testing model with AIC BIC criteria
mymodel <- Arima(organic$Ozone, order=c(3,1,1), include.constant = TRUE)
summary(mymodel)
mymodel <- Arima(organic$Ozone, order=c(0,1,1), include.constant = TRUE)
summary(mymodel)
## checking residuals 
u <- residuals(mymodel)
acf2(u)

## testing residuals ( white noise - ljung box)
Box.test(u, lag=1, type="Ljung-Box") # fitdf = p+q AND lag > fitdf

###### FORECASTING #######################################################################

fit <- forecast(mymodel)
plot(fit)


##########################################################################################
##########################################################################################
##################### MULTIVARIATE TIME SERIES ANALYSIS
##########################################################################################
##########################################################################################


##########################################################################################
### CHECKING FOR COINTEGRATION ####################################################
##########################################################################################

rlt <- lm(organic$AveragePrice ~ organic$Ozone )
summary(rlt)
u <- residuals(rlt)
plot.ts(u)
acf2(u)
testdf <- ur.df(u,type="drift",lags=2)
summary(testdf)

##########################################################################################
### Modele ARDL ####################################################
##########################################################################################

ardl <- dynlm(ts_organic ~ L(ts_organic,1) + ts_oz + L(ts_oz,1) )
summary(ardl)
# coeftest(ardl, vcov. = sandwich)
u <- residuals(ardl)
testdf <- ur.df(u,type="drift",lags=2)
summary(testdf)

# pred <- predict(ardl)
# plot(pred)

# xtable(summary(ardl))


##########################################################################################
### Modele VAR ####################################################
##########################################################################################



data <- ts.intersect(d_tso,d_oz)

VARselect(data , lag.max = 5)

Varmodel <- VAR(data, p=2, type = "const")
summary(Varmodel)


###### Validation ########################################################
serial.test(Varmodel, lags.pt = 3, type = "PT.asymptotic")
# test sur residus model 
# test portemanteau sur les residus la distribution asymtotic de ma stat de test
# cest chi deux du nombre de paprametre quon test - le nombre de parametre estim???s

###### Causalite ########################################################
Varmodel
causality(Varmodel, cause = c("d_tso"))
causality(Varmodel, cause = c("d_oz"))

###### Prediction ########################################################
pred <- predict(Varmodel, n.ahead = 4, ci = 0.95)
plot(pred)
fanchart(pred)

# Decomposition de la variance
fevd <- fevd(Varmodel, n.ahead = 10)
plot(fevd)


###### IRF ########################################################
irfprice <- irf(Varmodel, impulse = "d_tso", response = c( "d_tso", "d_oz"), n.ahead = 4, ortho=TRUE, boot = TRUE)
plot(irfprice)
irfoz <- irf(Varmodel, impulse = "d_oz", response = c( "d_tso", "d_oz"), n.ahead = 4, ortho=TRUE, boot = TRUE)
plot(irfoz)




##########################################################################################
##########################################################################################