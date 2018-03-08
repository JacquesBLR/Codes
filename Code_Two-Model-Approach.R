############################################
## Récupération des paramètres de demande ##
## en situation de monopole    #############
##
###################################
## Import des données #############
library(readxl)
library(lubridate)
library(plyr)


setwd("C:/Users/Bertrand/Documents/Competition_Policy_Cases/Codes/Codes")


# Import des données
Bicilandia <- read_excel("Damages.xls",
                         sheet="Bicilandia")
Bicilandia$Month = ymd(Bicilandia$Month)

Flandria <- read_excel("Damages.xls",
                       sheet="Flandria")
Flandria$Month <- ymd(Flandria$Month)

## Fin de l'import des données ########
#######################################
## Création des variables de volume ###
##
##
Bicilandia$Binda_Volume <- Bicilandia$Binda_Turnover / Bicilandia$Binda_Price
Bicilandia$Guerra_Volume <- Bicilandia$Guerra_Turnover/ Bicilandia$Guerra_Price
Bicilandia$Speicher_Volume <- Bicilandia$Speicher_Turnover / Bicilandia$Speicher_Price
##
Bicilandia$Cartel_Price <- (Bicilandia$Binda_Price + Bicilandia$Guerra_Price)/2
Bicilandia$Cartel_Cost <- (Bicilandia$Binda_Cost + Bicilandia$Guerra_Cost)/2
Bicilandia$Cartel_Volume <- (Bicilandia$Binda_Volume + Bicilandia$Guerra_Volume)/2
Bicilandia$asurb <- 2*Bicilandia$Cartel_Price - Bicilandia$Cartel_Cost
##
Bicilandia$Cartel_cf_Cournot_Price <- Bicilandia$asurb/3 + 2/3*Bicilandia$Cartel_Cost

Collusion <- Bicilandia
Collusion <- subset(Collusion, Month < ymd("2012-01-01"))
Collusion <- subset(Collusion, Month > ymd("2006-12-01"))
Collusion$Cartel_cf_Cournot_Price <- Collusion$asurb/3 + 2/3*Collusion$Cartel_Cost
Collusion$Cartel_cf_Cournot_Volume <- 4/3*Collusion$Cartel_Volume
##
plot(Bicilandia$Month, Bicilandia$Binda_Volume , type = "l" , col = "Blue", ylab = "Volume", mar = c(0,0,0,0), main = "Volume Bicilandia with Counterfactual Cournot Model", ylim = c(10000,65000))
lines(Bicilandia$Month, Bicilandia$Guerra_Volume, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Volume, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(Collusion$Month,Collusion$Cartel_cf_Cournot_Volume, col = "brown", lty = 2)
##
plot(Bicilandia$Month, Bicilandia$Binda_Price , type = "l" , col = "Blue",ylim = c(10,20), ylab = "Price", xlab = "", oma = c(0,0,0,0), main = "Prices Bicilandia with counterfactual Cournot Model")
lines(Bicilandia$Month, Bicilandia$Guerra_Price, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Price, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(Collusion$Month,Collusion$Cartel_cf_Cournot_Price, col = "brown", lty = 2)
