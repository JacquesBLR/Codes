
install.packages("readxl")
install.packages("lubridate")
install.packages("plyr")

library(readxl)
library(lubridate)
library(plyr)


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
##### Dif In Dif 1 avec Cross-Sectional : Speicher ####
#######################################################


# Création Dummy période de collusion

Bicilandia$collusion <- ifelse( ymd("2006-12-01") < Bicilandia$Month & Bicilandia$Month < ymd("2011-08-01"), 1 , 0)

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
DifInDif1 <- lm(Price ~ Cost + collusion + Cartel + collusion * Cartel , data = BicilandiaDataModel1)
summary.lm(DifInDif1)

# Calcul du contrefactuel
Bicilandia$CounterCartelPrice = DifInDif1$coefficients[1] + DifInDif1$coefficients[2]*Bicilandia$Cartel_Cost + DifInDif1$coefficients[4]
Bicilandia$CounterSpeicherPrice = DifInDif1$coefficients[1] + DifInDif1$coefficients[2]*Bicilandia$Speicher_Cost

# Sortie graphique
plot(Bicilandia$Month, Bicilandia$CounterCartelPrice , type = "l" , col = "Blue", ylab = "Price", mar = c(0,0,0,0), main = "Price Bicilandia with Counterfactual Cournot Model", ylim = c(5,25))
lines(Bicilandia$Month, Bicilandia$Cartel_Price, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$CounterSpeicherPrice, type = "l", col = "Red")
lines(Bicilandia$Month, Bicilandia$Speicher_Price, type = "l", col = "Brown")

########################################
###### TEST Modèle d'offre #############
########################################

Cartel$LogQuantity = log(Cartel$Quantity)
Cartel$LogPrice = log(Cartel$Price)
Cartel$LogCost = log(Cartel$Cost)

ModOffre0 = lm(LogQuantity ~ LogPrice + LogCost, data= Cartel)
summary.lm(ModOffre0)

Elasticity0 = ModOffre0$coefficients

Elasticity0

ModOffre1 = lm(LogPrice ~ LogCost + collusion, data= Cartel)
summary.lm(ModOffre1)

Cartel$IV1 = predict(lm(LogPrice ~ LogCost + collusion, data= Cartel))

ModOffre2 = lm(LogQuantity ~ LogCost + IV1, data= Cartel)

Elasticity2 = ModOffre2$coefficients

Elasticity2

#######################################################
##### Dif In Dif 2 avec Cross-Sectional : Flandria ####
#######################################################


# Création Dummy période de collusion

Flandria$collusion <- ifelse( ymd("2006-12-01") < Flandria$Month & Flandria$Month < ymd("2011-08-01"), 1 , 0)

# Création d'indicateurs moyens entre les Binda et Guerra

Flandria$Cartel_Price = (Flandria$Binda_Price + Flandria$Guerra_Price) / 2
Flandria$Cartel_Cost = (Flandria$Binda_Cost + Flandria$Guerra_Cost) / 2
Flandria$Cartel_Turnover = (Flandria$Binda_Turnover + Flandria$Guerra_Turnover) / 2

# Création table Cartel Flandria
CartelFL = Flandria[,c("Month", "collusion", "Cartel_Price", "Cartel_Cost" ,"Cartel_Turnover")]
CartelFL = rename(CartelFL,c("Cartel_Price"="Price", "Cartel_Cost"="Cost", "Cartel_Turnover"="Turnover"))
CartelFL$Bicilinda = 0

# Création table Flandria
Cartel$Cartel = NULL
Cartel$Bicilinda = 1

# Empilement des données

BicilandiaDataModel2 = rbind(Cartel,CartelFL)

# Modèle Dif In Dif avec groupe de contrôle Before-After : Avant et Après VS Periode de collusion et Cross-sectional  : Speicher
DifInDif2 <- lm(Price ~ Cost + collusion + Bicilinda + collusion * Bicilinda, data = BicilandiaDataModel2)
summary.lm(DifInDif2)
