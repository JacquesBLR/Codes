
install.packages("readxl")
install.packages("lubridate")
install.packages("plyr")

library(readxl)
library(lubridate)
library(plyr)
library(sqldf)

#setwd("C:/Users/Bertrand/Documents/Competition_Policy_Cases")
setwd("/Users/jacquesaguilera/GDrive/Ensae/Scolarite_Ensae/Cases Hypothetical damages/Damages")

# Import des données
Bicilandia <- read_excel("Damages.xlsx",
           sheet="Bicilandia")
Bicilandia$Month = ymd(Bicilandia$Month)

Flandria <- read_excel("Damages.xlsx",
           sheet="Flandria")
Flandria$Month <- ymd(Flandria$Month)



#######################################################
############### Before - After Cartel #################
#######################################################

# Création Dummy période de collusion

Bicilandia$collusion <- ifelse( ymd("2006-12-01") < Bicilandia$Month & Bicilandia$Month < ymd("2011-12-01"), 1 , 0)

# Création d'indicateurs moyens entre les Binda et Guerra

Bicilandia$Cartel_Price = (Bicilandia$Binda_Price + Bicilandia$Guerra_Price) / 2
Bicilandia$Cartel_Cost = (Bicilandia$Binda_Cost + Bicilandia$Guerra_Cost) / 2
Bicilandia$Cartel_Turnover = (Bicilandia$Binda_Turnover + Bicilandia$Guerra_Turnover) / 2

Bicilandia$Cartel_Quantity = (Bicilandia$Binda_Quantity + Bicilandia$Guerra_Quantity) / 2


# Modèle Before-After avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion
Before_After <- lm(Cartel_Price ~ Cartel_Cost + collusion , data = Bicilandia)
summary.lm(Before_After)

Before_After$coefficients
# Calcul du contrefactuel
Bicilandia$CounterCartelPrice0 = Before_After$coefficients[1] + Before_After$coefficients[2]*Bicilandia$Cartel_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelPrice0 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia ", ylim = c(5,25))
lines(Bicilandia$Month, Bicilandia$Cartel_Price, type = "l", col = "Green")



#######################################################
##### Dif In Dif 1 avec Cross-Sectional : Speicher ####
#######################################################


# Création Dummy période de collusion

Bicilandia$collusion <- ifelse( ymd("2006-12-01") < Bicilandia$Month & Bicilandia$Month < ymd("2011-12-01"), 1 , 0)

# Création d'indicateurs moyens entre les Binda et Guerra

Bicilandia$Cartel_Price = (Bicilandia$Binda_Price + Bicilandia$Guerra_Price) / 2
Bicilandia$Cartel_Cost = (Bicilandia$Binda_Cost + Bicilandia$Guerra_Cost) / 2
Bicilandia$Cartel_Turnover = (Bicilandia$Binda_Turnover + Bicilandia$Guerra_Turnover) / 2

Bicilandia$Cartel_Quantity = (Bicilandia$Binda_Quantity + Bicilandia$Guerra_Quantity) / 2

# Création table Cartel
Cartel = Bicilandia[,c("Month", "collusion", "Cartel_Price", "Cartel_Cost" ,"Cartel_Turnover", "Cartel_Quantity")]
Cartel = rename(Cartel,c("Cartel_Price"="Price", "Cartel_Cost"="Cost", "Cartel_Turnover"="Turnover", "Cartel_Quantity"="Quantity"))
Cartel$Cartel = 1

# Création table Speicher
Speicher = Bicilandia[,c("Month", "collusion", "Speicher_Price", "Speicher_Cost" ,"Speicher_Turnover", "Speicher_Quantity")]
Speicher = rename(Speicher,c("Speicher_Price"="Price", "Speicher_Cost"="Cost", "Speicher_Turnover"="Turnover","Speicher_Quantity"="Quantity"))
Speicher$Cartel = 0

# Empilement des données

BicilandiaDataModel1 = rbind(Cartel,Speicher)

# Modèle Dif In Dif avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion et Cross-sectional  : Speicher
DifInDifPrice1 <- lm(Price ~ Cost + collusion + Cartel + collusion * Cartel , data = BicilandiaDataModel1)
summary.lm(DifInDifPrice1)

# Calcul du contrefactuel
Bicilandia$CounterCartelPrice1 = DifInDifPrice1$coefficients[1] + DifInDifPrice1$coefficients[2]*Bicilandia$Cartel_Cost + DifInDifPrice1$coefficients[4]
Bicilandia$CounterSpeicherPrice = DifInDifPrice1$coefficients[1] + DifInDifPrice1$coefficients[2]*Bicilandia$Speicher_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelPrice1 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia ", ylim = c(5,25))
#lines(Bicilandia$Month, Bicilandia$CounterCartelPrice0, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Cartel_Price, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$CounterSpeicherPrice, type = "l", col = "Red")
lines(Bicilandia$Month, Bicilandia$Speicher_Price, type = "l", col = "Brown")

# Calcul du dommage Model 1

Dommage1A = sqldf("select sum((Cartel_Price - CounterCartelPrice1)*Cartel_Quantity*collusion)
from Bicilandia")

Dommage1B = sqldf("select sum((Speicher_Price - CounterSpeicherPrice)*Speicher_Quantity*collusion)
from Bicilandia")

Dommage1AB = Dommage1A + Dommage1B

# Modèle Dif In Dif avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion et Cross-sectional  : Speicher
DifInDifQty1 <- lm(Quantity ~ Cost + collusion + Cartel + collusion * Cartel , data = BicilandiaDataModel1)
summary.lm(DifInDifQty1)

# Calcul du contrefactuel
Bicilandia$CounterCartelQuantity1 = DifInDifQty1$coefficients[1] + DifInDifQty1$coefficients[2]*Bicilandia$Cartel_Cost + DifInDifQty1$coefficients[4]
Bicilandia$CounterSpeicherQuantity = DifInDifQty1$coefficients[1] + DifInDifQty1$coefficients[2]*Bicilandia$Speicher_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelQuantity1 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Quantity Bicilandia ", ylim = c(20000,70000))
#lines(Bicilandia$Month, Bicilandia$CounterCartelPrice0, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Cartel_Quantity, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$CounterSpeicherQuantity, type = "l", col = "Red")
lines(Bicilandia$Month, Bicilandia$Speicher_Quantity, type = "l", col = "Brown")



#######################################################
##### Dif In Dif 2 avec Cross-Sectional : Flandria ####
#######################################################


# Création Dummy période de collusion

Flandria$collusion <- ifelse( ymd("2006-12-01") < Flandria$Month & Flandria$Month < ymd("2011-12-01"), 1 , 0)

Flandria$Binda_Quantity  = Flandria$Binda_Turnover / Flandria$Binda_Price

Flandria$Guerra_Quantity = Flandria$Guerra_Turnover / Flandria$Guerra_Price

# Création d'indicateurs moyens entre les Binda et Guerra

Flandria$Cartel_Price = (Flandria$Binda_Price + Flandria$Guerra_Price) / 2
Flandria$Cartel_Cost = (Flandria$Binda_Cost + Flandria$Guerra_Cost) / 2
Flandria$Cartel_Turnover = (Flandria$Binda_Turnover + Flandria$Guerra_Turnover) / 2

Flandria$Cartel_Quantity = (Flandria$Binda_Quantity + Flandria$Guerra_Quantity) / 2


# Création table Cartel Flandria
CartelFL = Flandria[,c("Month", "collusion", "Cartel_Price", "Cartel_Cost" ,"Cartel_Turnover", "Cartel_Quantity")]
CartelFL = rename(CartelFL,c("Cartel_Price"="Price", "Cartel_Cost"="Cost", "Cartel_Turnover"="Turnover", "Cartel_Quantity"="Quantity"))
CartelFL$Bicilinda = 0

# Création table Flandria
Cartel$Cartel = NULL
Cartel$Bicilinda = 1

# Empilement des données

BicilandiaDataModel2 = rbind(Cartel,CartelFL)

# Modèle Dif In Dif avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion et Cross-sectional  : Speicher
DifInDifPrice2 <- lm(Price ~ Cost + collusion + Bicilinda + collusion * Bicilinda, data = BicilandiaDataModel2)
summary.lm(DifInDifPrice2)


# Calcul du contrefactuel
Bicilandia$CounterCartelPrice2 = DifInDifPrice2$coefficients[1] + DifInDifPrice2$coefficients[2]*Bicilandia$Cartel_Cost + DifInDifPrice2$coefficients[4]
# Bicilandia$CounterSpeicherPrice = DifInDifPrice2$coefficients[1] + DifInDifPrice2$coefficients[2]*Bicilandia$Speicher_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelPrice2 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia ", ylim = c(5,25))
lines(Bicilandia$Month, Bicilandia$CounterCartelPrice1, type = "l", col = "Red")
lines(Bicilandia$Month, Bicilandia$Cartel_Price, type = "l", col = "Green")

# Calcul du dommage Model 2

Dommage2A = sqldf("select sum((Cartel_Price - CounterCartelPrice2)*Cartel_Quantity*collusion)
from Bicilandia")


Dommage2AB = Dommage2A + Dommage1B

# Modèle Dif In Dif avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion et Cross-sectional  : Speicher
DifInDifQty2 <- lm(Quantity ~ Cost + collusion + Bicilinda + collusion * Bicilinda, data = BicilandiaDataModel2)
summary.lm(DifInDifQty2)


# Calcul du contrefactuel
Bicilandia$CounterCartelQuantity2 = DifInDifQty2$coefficients[1] + DifInDifQty2$coefficients[2]*Bicilandia$Cartel_Cost + DifInDifQty2$coefficients[4]
# Bicilandia$CounterSpeicherPrice = DifInDifPrice2$coefficients[1] + DifInDifPrice2$coefficients[2]*Bicilandia$Speicher_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelQuantity2 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia ", ylim = c(20000,70000))
#lines(Bicilandia$Month, Bicilandia$CounterCartelPrice0, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Cartel_Quantity, type = "l", col = "Green")




# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelQuantity2 , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia ", ylim = c(20000,70000))
lines(Bicilandia$Month, Bicilandia$CounterCartelQuantity1, type = "l", col = "Red")
lines(Bicilandia$Month, Bicilandia$Cartel_Quantity, type = "l", col = "Green")

















######################################################
########## TEST Modèle Time Series ###################
######################################################

install.packages("tseries")
library(tseries)

plot.ts(Bicilandia$Cartel_Cost, plot.type = "single", col = c("red"))

par(mfrow=c(1,1))
acf(Bicilandia$Cartel_Cost, lag.max = 100,type = "correlation",plot = TRUE,  demean = TRUE, na.action = na.pass)
acf(Bicilandia$Cartel_Cost, lag.max = 100,type = "partial",plot = TRUE,  demean = TRUE, na.action = na.pass)


fit_ar1=arima(Bicilandia$Cartel_Cost, c(1,0,0))
fit_ma2=arima(Bicilandia$Cartel_Cost, c(0,0,2))

res_fit_ar1 = fit_ar1$residuals

par(mfrow=c(1,2))
acf(res_fit_ar1, lag.max = 50,type = "correlation",plot = TRUE,  demean = TRUE, na.action = na.pass)
acf(res_fit_ar1, lag.max = 50,type = "partial",plot = TRUE, na.action = na.pass)

fit_ar1$coef[1]

install.packages("forecast")

library(forecast)

fcast <- forecast(fit_ar1, 300)
plot(fcast)

predictprice = Bicilandia$Cartel_Price * fit_ar1$coefficients[1]
plot.ts(predictprice, plot.type = "single", col = c("red"))



install.packages("forecast")
library(forecast)
trend_cost = ma(Bicilandia$Cartel_Cost, order = 8, centre = T)
plot(Bicilandia$Cartel_Price-5, ylim = c(5,25))
lines(Bicilandia$Cartel_Cost)
plot(as.ts(trend_cost))
