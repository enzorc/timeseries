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

# variable crea

data(nporg)
# il nous faut sélectionner la série Real Per Capita GNP de cette base,
# après une petite recherche sur le net on trouve des détails sur chacune
# des variables et la valeurs qui nous intéresse est gnp.pc : Real Per Capita GNP, [1958 Dollars], [1909 -- 1970]

t <- 1:(length(nporg$year))
nporg$year <- as.Date(as.character(nporg$year), format = "%Y")
rownames(nporg) <- nporg$year
loggnp <- na.omit(log(nporg$gnp.pc))
logwg <- na.omit(log(nporg$wg.r))
nporg2 <- na.omit(nporg)





## EXO 1 
### Consid´erez la s´ erie Real Per Capita GNP. Analyser la s´ erie (graphique,
### autocorr´ elations).

# Analyser les données


plot.ts(nporg$gnp.pc)
plot.ts(loggnp)

summary(nporg$gnp.pc)
summary(loggnp)

acf2(nporg$gnp.pc, na.action = na.pass)
acf2(loggnp, na.action = na.pass)


## EXO 2
### Pour cette variable, pouvez-vous r ´ epliquer les tests de racine unit ´e de la Table 5 ?
### (Attention `a la notation pour le nombre de retards k utilis ´e dans l'article, qui ne
### correspond pas `a celle utilise dans le cours. k = 2 correspond `a notre mod` ele
### avec Yt????1 comme seul retard).


test.tgnp1 <- ur.df(loggnp, type = "drift" , lags =1)
test.tgnp2 <- ur.df(loggnp, type = "trend", lags =1)
test.tgnp3 <- ur.df(loggnp, type = "none" , lags =1)

summary(test.tgnp1)
summary(test.tgnp2)
summary(test.tgnp3)


# EXO 3
### Quelles conclusions tirez-vous de ces tests?






# EXO 4
### Essayer d'introduire plus de retards dans la r ´egression. Vos r ´ esultats
### confirment-ils le choix de Nelson et Plosser ? Si ce n'est pas le cas, est-ce que
### vos conclusions sont affect ´ees pour votre choix pr ´ ef ´ er ´e du nombre de retards?

test.tgnp2l <- ur.df(nporg2$gnp.pc, type = "trend", lags =2)
test.tgnp3l <- ur.df(nporg2$gnp.pc, type = "trend", lags =3)
test.tgnp4l <- ur.df(nporg2$gnp.pc, type = "trend", lags =4)

test.tgnpauto <- ur.df(nporg2$gnp.pc, type = "trend", selectlags =c("Fixed", "AIC", "BIC"))

summary(test.tgnp2l)
summary(test.tgnp3l)
summary(test.tgnp4l)

summary(test.tgnpauto)



# EXO 5
#Choississez une autre des variables de la base et r ´ep´etez l'analyse pr ´ec´edente.

#??? ANALYSE

plot.ts(nporg$wg.r)
plot.ts(logwg)

summary(nporg$wg.r)
summary(logwg)

acf2(nporg$wg.r, na.action = na.pass)
acf2(logwg, na.action = na.pass)

test.twg1l <- ur.df(logwg, type = "trend", lags =1)
test.twg2l <- ur.df(logwg, type = "trend", lags =2)
test.twg3l <- ur.df(logwg, type = "trend", lags =3)
test.twg4l <- ur.df(logwg, type = "trend", lags =4)

test.twgauto <- ur.df(logwg, type = "trend", selectlags = c("Fixed", "AIC", "BIC"))



summary(test.twg1l)
summary(test.twg2l)
summary(test.twg3l)
summary(test.twg4l)

summary(test.twgauto)





