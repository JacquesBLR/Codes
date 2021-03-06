####################################
## Modèle d'utilité Logit ##########
####################################
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
Bicilandia$Total_Volume <- Bicilandia$Binda_Volume + Bicilandia$Guerra_Volume + Bicilandia$Speicher_Volume
##Market_Size <- max(Bicilandia$Total_Volume) + 5
Market_Size <- 150000
##
Bicilandia$Binda_Share <- Bicilandia$Binda_Volume/Market_Size
Bicilandia$Guerra_Share <- Bicilandia$Guerra_Volume / Market_Size
Bicilandia$Speicher_Share <- Bicilandia$Speicher_Volume / Market_Size
##
## On définit l'Outside Option
Bicilandia$Outside_Share <- (1 - Bicilandia$Binda_Share - Bicilandia$Guerra_Share -Bicilandia$Speicher_Share)
##
Bicilandia$ln_Binda_Share <- log((Bicilandia$Binda_Share/Bicilandia$Outside_Share),base = exp(1))
Bicilandia$ln_Guerra_Share <- log((Bicilandia$Guerra_Share/Bicilandia$Outside_Share), base = exp(1))
Bicilandia$ln_Speicher_Share <- log((Bicilandia$Speicher_Share/Bicilandia$Outside_Share), base = exp(1))
## fin des créations de variables ######
########################################
## Régression modèle logit #############
logitBinda <- Bicilandia[,c("Month","ln_Binda_Share","Binda_Price","Binda_Cost","Binda_Share","Binda_Volume","Total_Volume")]
colnames(logitBinda) <- c("Month","ln_Share","Price","Cost","Share","Volume","Total_Volume")
logitBinda$BindaGuerraQuality <- 1
logitGuerra <- Bicilandia[,c("Month","ln_Guerra_Share","Guerra_Price","Guerra_Cost","Guerra_Share","Guerra_Volume","Total_Volume")]
colnames(logitGuerra) <- c("Month","ln_Share","Price","Cost","Share","Volume","Total_Volume")
logitGuerra$BindaGuerraQuality <- 1
logitSpeicher <- Bicilandia[,c("Month","ln_Speicher_Share","Speicher_Price","Speicher_Cost","Speicher_Share","Speicher_Volume","Total_Volume")]
colnames(logitSpeicher) <- c("Month","ln_Share","Price","Cost","Share","Volume","Total_Volume")
logitSpeicher$BindaGuerraQuality <- 0


##
library(AER)
modelelogit <- ivreg(ln_Share ~ Price | Cost , data = logitm)
modele1ststep <- lm(Price ~ Cost, data = logitm)
logitm$Price_hat <- modele1ststep$fitted.values
Modele2dstep <- lm(ln_Share ~ Price_hat, data = logitm)
summary(modelelogit)
summary(Modele2dstep)
##
mu <- -(modelelogit$coefficients[[2]])
logitm$simulated_Cost <- logitm$Price - 1 - exp(logitm$ln_Share)
##
## Les coûts simulés ne sont pas inintéressants, mais guère utiles dans le cas présent.
## On passe dans une phase de construction d'un contrefactuel "Cournot" sur la période de collusion.

####################################################################
## Construction de la fonction permettant d'inverser une fonction ##
inverse <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(0.01,12))$root[1])
}
############
## On récupère maintenant les données observées sur la période de collusion
## On filtre sur la période avec collusion
logitm <- rbind(logitBinda,logitGuerra,logitSpeicher)
logitm <- subset(logitm, Month < ymd("2012-07-01"))
logitm <- subset(logitm, Month > ymd("2006-12-01"))
##
library(AER)
modelelogit <- ivreg(ln_Share ~ Price | Cost , data = logitm)
summary(modelelogit)
## Construction du modèle contrefactuel
contrefactual <- Bicilandia[25:90,]
contrefactual$Binda_Residuals <- modelelogit$residuals[1:66]
contrefactual$Guerra_Residuals <- modelelogit$residuals[67:132]
contrefactual$Speicher_Residuals <- modelelogit$residuals[133:198]
alpha <- modelelogit$coefficients[[1]]
mu <- -modelelogit$coefficients[[2]]
inverse <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(0.0000001,20))$root[1])
}
fmu <- function(x) {
  return(log(x)/mu + x)
}
gmu <- function(y) {
  return(inverse(fmu,y))
}
contrefactual$Binda_cf_Volumes <- rep(0, length(contrefactual$Binda_Cost))
for (i in 1:length(contrefactual$Binda_Price)) {
  contrefactual$Binda_cf_Volumes[i] <- Market_Size*gmu((alpha+contrefactual$Binda_Residuals[i])/mu - 1 - contrefactual$Binda_Cost[i])/(1+gmu((alpha+contrefactual$Binda_Residuals[i])/mu - 1 - contrefactual$Binda_Cost[i]) + gmu((alpha+contrefactual$Guerra_Residuals[i])/mu - 1 - contrefactual$Guerra_Cost[i])+gmu((alpha+contrefactual$Speicher_Residuals[i])/mu - 1 - contrefactual$Speicher_Cost[i]))
}
contrefactual$Guerra_cf_Volumes <- rep(0, length(contrefactual$Binda_Cost))
for (i in 1:length(contrefactual$Binda_Price)) {
  contrefactual$Guerra_cf_Volumes[i] <- Market_Size*gmu((alpha+contrefactual$Guerra_Residuals[i])/mu - 1 - contrefactual$Guerra_Cost[i])/(1+gmu((alpha+contrefactual$Binda_Residuals[i])/mu - 1 - contrefactual$Binda_Cost[i]) + gmu((alpha+contrefactual$Guerra_Residuals[i])/mu - 1 - contrefactual$Guerra_Cost[i])+gmu((alpha+contrefactual$Speicher_Residuals[i])/mu - 1 - contrefactual$Speicher_Cost[i]))
}
contrefactual$Speicher_cf_Volumes <- rep(0, length(contrefactual$Binda_Cost))
for (i in 1:length(contrefactual$Binda_Price)) {
  contrefactual$Speicher_cf_Volumes[i] <- Market_Size*gmu((alpha+contrefactual$Speicher_Residuals[i])/mu - 1 - contrefactual$Speicher_Cost[i])/(1+gmu((alpha+contrefactual$Binda_Residuals[i])/mu - 1 - contrefactual$Binda_Cost[i]) + gmu((alpha+contrefactual$Guerra_Residuals[i])/mu - 1 - contrefactual$Guerra_Cost[i])+gmu((alpha+contrefactual$Speicher_Residuals[i])/mu - 1 - contrefactual$Speicher_Cost[i]))
}
contrefactual$Binda_cf_Price <- contrefactual$Binda_Cost + 1 - contrefactual$Binda_cf_Volumes/(contrefactual$Binda_cf_Volumes + contrefactual$Guerra_cf_Volumes + contrefactual$Speicher_cf_Volumes - Market_Size)
contrefactual$Guerra_cf_Price <- contrefactual$Guerra_Cost + 1 - contrefactual$Guerra_cf_Volumes/(contrefactual$Binda_cf_Volumes + contrefactual$Guerra_cf_Volumes + contrefactual$Speicher_cf_Volumes - Market_Size)
contrefactual$Speicher_cf_Price <- contrefactual$Speicher_Cost + 1 - contrefactual$Speicher_cf_Volumes/(contrefactual$Binda_cf_Volumes + contrefactual$Guerra_cf_Volumes + contrefactual$Speicher_cf_Volumes - Market_Size)
##
## Construction d'un contrefactuel répondant à la concurrence Bertrand
inverse1 <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(1e-8,1-1e-11))$root[1])
}
inverse2 <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(1e-7,1 - 1e-10))$root[1])
}
f_bertrand <- function(s) {
  return(1/(1-s) + log(s))
}
inverse_f_bertrand <- function(y) {
  return(inverse1(f_bertrand,y))
}
s0 <- rep(0,length(contrefactual$Month))
s_Binda <- rep(0,length(contrefactual$Month))
s_Guerra <- rep(0,length(contrefactual$Month))
s_Speicher <- rep(0,length(contrefactual$Month))

for (i in (1:length(contrefactual$Month))) {
  g_bertrand <- function(s) {
    return(s + inverse_f_bertrand(alpha + log(s) + contrefactual$Binda_Residuals[i] - mu*contrefactual$Binda_Cost[i]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Guerra_Residuals[i] - mu*contrefactual$Guerra_Cost[i]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Speicher_Residuals[i] - mu*contrefactual$Speicher_Cost[i]))
  }
  s0[i]  <- inverse2(g_bertrand,1)
  s_Binda[i] <- inverse_f_bertrand(alpha + log(s0[i]) + contrefactual$Binda_Residuals[i]-mu*contrefactual$Binda_Cost[i])
  s_Guerra[i] <- inverse_f_bertrand(alpha + log(s0[i]) + contrefactual$Guerra_Residuals[i]-mu*contrefactual$Guerra_Cost[i])
  s_Speicher[i] <- inverse_f_bertrand(alpha + log(s0[i]) + contrefactual$Speicher_Residuals[i]-mu*contrefactual$Speicher_Cost[i])
}
g_bertrand <- function(s) {
  return(s + inverse_f_bertrand(alpha + log(s) + contrefactual$Binda_Residuals[14] - mu*contrefactual$Binda_Cost[14]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Guerra_Residuals[14] - mu*contrefactual$Guerra_Cost[14]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Speicher_Residuals[14] - mu*contrefactual$Speicher_Cost[14]))
}
inverse2(g_bertrand,1)
## Le modèle Bertrand donne des résultats extrêmes ###
######################################################

plot(Bicilandia$Month, Bicilandia$Binda_Volume , type = "l" , col = "Blue", ylab = "Volume", mar = c(0,0,0,0), main = "Counterfactual Volumes with One-Model Approach", ylim = c(10000,65000))
lines(Bicilandia$Month, Bicilandia$Guerra_Volume, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Volume, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(contrefactual$Month,contrefactual$Binda_cf_Volumes, col = "blue", lty = 2)
lines(contrefactual$Month,contrefactual$Guerra_cf_Volumes, col = "green", lty = 2)
lines(contrefactual$Month,contrefactual$Speicher_cf_Volumes, col = "Red", lty = 2)
##
plot(Bicilandia$Month, Bicilandia$Binda_Price , type = "l" , col = "Blue",ylim = c(10,20), ylab = "Price", xlab = "", oma = c(0,0,0,0), main = "Counterfactual Prices with One-Model Approach")
lines(Bicilandia$Month, Bicilandia$Guerra_Price, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Price, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(contrefactual$Month,contrefactual$Binda_cf_Price, col = "blue", lty = 2)
lines(contrefactual$Month,contrefactual$Guerra_cf_Price, col = "green", lty = 2)
lines(contrefactual$Month,contrefactual$Speicher_cf_Price, col = "Red", lty = 2)
##
############################################################
## Calcul des dommages-intérêts                   ##########
# r : taux d'intérêt annuel sur la période de collusion.  ##
Aujourdhui <- ymd("2017-01-01")


Damages_paidunits_Binda <- function(r) {
  return(sum((contrefactual$Binda_Price - contrefactual$Binda_cf_Price)*contrefactual$Binda_Volume*((1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))))
}
Damages_paidunits_Guerra <- function(r) {
  return(sum((contrefactual$Guerra_Price - contrefactual$Guerra_cf_Price)*contrefactual$Guerra_Volume*((1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))))
}
Damages_paidunits_Binda(0.015)
Damages_paidunits_Guerra(0.015)
Profits_Binda <- function(r) {
  sum((contrefactual$Binda_Price-contrefactual$Binda_Cost)*contrefactual$Binda_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Profits_Binda(0.015)

Harm_Binda <- function(r) {
  sum((contrefactual$Binda_cf_Volumes - contrefactual$Binda_Volume)*contrefactual$Binda_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Harm_Binda(0.015)
Harm_Guerra <- function(r) {
  sum((contrefactual$Guerra_cf_Volumes - contrefactual$Guerra_Volume)*contrefactual$Guerra_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Profits_Guerra <- function(r) {
  sum((contrefactual$Guerra_Price-contrefactual$Guerra_Cost)*contrefactual$Guerra_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Profits_Guerra(0.015)

Cf_Profits_Binda <- function(r) {
  sum((contrefactual$Binda_cf_Price - contrefactual$Binda_Cost)*contrefactual$Binda_cf_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Cf_Profits_Binda(0.015)

Cf_Profits_Guerra <- function(r) {
  sum((contrefactual$Guerra_cf_Price - contrefactual$Guerra_Cost)*contrefactual$Guerra_cf_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Cf_Profits_Guerra(0.015)

Harm_Speicher <- function(r) {
  sum((contrefactual$Speicher_cf_Volumes - contrefactual$Speicher_Volume)*contrefactual$Speicher_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual$Month)/365))
}
Harm_Speicher(0.015)
##########
## Modèle personnalisé de demande
logit_Bindam <- subset(logitBinda, Month < ymd("2012-07-01"))
logit_Bindam <- subset(logit_Bindam, Month > ymd("2006-12-01"))
modelelogit_Bindam <- ivreg(ln_Share ~ Price | Cost , data = logit_Bindam)
summary(modelelogit_Bindam)
logit_Guerram <- subset(logitGuerra, Month < ymd("2012-07-01"))
logit_Guerram <- subset(logit_Guerram, Month > ymd("2006-12-01"))
modelelogit_Guerram <- ivreg(ln_Share ~ Price | Cost , data = logit_Guerram)
summary(modelelogit_Guerram)
logit_Speicherm <- subset(logitSpeicher, Month < ymd("2012-07-01"))
logit_Speicherm <- subset(logit_Speicherm, Month > ymd("2006-12-01"))
modelelogit_Speicherm <- ivreg(ln_Share ~ Price | Cost , data = logit_Speicherm)
summary(modelelogit_Speicherm)
##
contrefactual_differencie <- Bicilandia[25:90,]
contrefactual_differencie$Binda_Residuals <- modelelogit_Bindam$residuals[1:66]
contrefactual_differencie$Guerra_Residuals <- modelelogit_Guerram$residuals[1:66]
contrefactual_differencie$Speicher_Residuals <- modelelogit_Speicherm$residuals[1:66]

alpha_Binda <- modelelogit_Bindam$coefficients[[1]]
mu_Binda <- -modelelogit_Bindam$coefficients[[2]]
alpha_Guerra <- modelelogit_Guerram$coefficients[[1]]
mu_Guerra <- -modelelogit_Guerram$coefficients[[2]]
alpha_Speicher <- modelelogit_Speicherm$coefficients[[1]]
mu_Speicher <- -modelelogit_Speicherm$coefficients[[2]]

inverse <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(0.0000001,30))$root[1])
}
fmu_Binda <- function(x) {
  return(log(x)/mu_Binda + x)
}
gmu_Binda <- function(y) {
  return(inverse(fmu_Binda,y))
}
fmu_Guerra <- function(x) {
  return(log(x)/mu_Guerra + x)
}
gmu_Guerra <- function(y) {
  return(inverse(fmu_Guerra,y))
}
fmu_Speicher <- function(x) {
  return(log(x)/mu_Speicher + x)
}
gmu_Speicher <- function(y) {
  return(inverse(fmu_Speicher,y))
}
for (i in 1:length(contrefactual_differencie$Binda_Price)) {
  contrefactual_differencie$Binda_cf_Volumes[i] <- Market_Size*gmu_Binda((alpha_Binda+contrefactual_differencie$Binda_Residuals[i])/mu_Binda - 1 - contrefactual_differencie$Binda_Cost[i])/(1+gmu_Binda((alpha_Binda+contrefactual_differencie$Binda_Residuals[i])/mu_Binda - 1 - contrefactual_differencie$Binda_Cost[i]) + gmu_Guerra((alpha_Guerra+contrefactual_differencie$Guerra_Residuals[i])/mu_Guerra - 1 - contrefactual_differencie$Guerra_Cost[i])+gmu_Speicher((alpha_Speicher+contrefactual_differencie$Speicher_Residuals[i])/mu_Speicher - 1 - contrefactual_differencie$Speicher_Cost[i]))
}
for (i in 1:length(contrefactual_differencie$Binda_Price)) {
  contrefactual_differencie$Guerra_cf_Volumes[i] <- Market_Size*gmu_Guerra((alpha_Guerra+contrefactual_differencie$Guerra_Residuals[i])/mu_Guerra - 1 - contrefactual_differencie$Guerra_Cost[i])/(1+gmu_Binda((alpha_Binda+contrefactual_differencie$Binda_Residuals[i])/mu_Binda - 1 - contrefactual_differencie$Binda_Cost[i]) + gmu_Guerra((alpha_Guerra+contrefactual_differencie$Guerra_Residuals[i])/mu_Guerra - 1 - contrefactual_differencie$Guerra_Cost[i])+gmu_Speicher((alpha_Speicher+contrefactual_differencie$Speicher_Residuals[i])/mu_Speicher - 1 - contrefactual_differencie$Speicher_Cost[i]))
}
for (i in 1:length(contrefactual_differencie$Binda_Price)) {
  contrefactual_differencie$Speicher_cf_Volumes[i] <- Market_Size*gmu_Speicher((alpha_Speicher+contrefactual_differencie$Speicher_Residuals[i])/mu_Speicher - 1 - contrefactual_differencie$Speicher_Cost[i])/(1+gmu_Binda((alpha_Binda+contrefactual_differencie$Binda_Residuals[i])/mu_Binda - 1 - contrefactual_differencie$Binda_Cost[i]) + gmu_Guerra((alpha_Guerra+contrefactual_differencie$Guerra_Residuals[i])/mu_Guerra - 1 - contrefactual_differencie$Guerra_Cost[i])+gmu_Speicher((alpha_Speicher+contrefactual_differencie$Speicher_Residuals[i])/mu_Speicher - 1 - contrefactual_differencie$Speicher_Cost[i]))
}
contrefactual_differencie_differencie$Binda_cf_Price <- contrefactual_differencie_differencie$Binda_Cost + 1 - contrefactual_differencie_differencie$Binda_cf_Volumes/(contrefactual_differencie_differencie$Binda_cf_Volumes + contrefactual_differencie_differencie$Guerra_cf_Volumes + contrefactual_differencie_differencie$Speicher_cf_Volumes - Market_Size)
contrefactual_differencie_differencie$Guerra_cf_Price <- contrefactual_differencie_differencie$Guerra_Cost + 1 - contrefactual_differencie_differencie$Guerra_cf_Volumes/(contrefactual_differencie_differencie$Binda_cf_Volumes + contrefactual_differencie_differencie$Guerra_cf_Volumes + contrefactual_differencie_differencie$Speicher_cf_Volumes - Market_Size)
contrefactual_differencie_differencie$Speicher_cf_Price <- contrefactual_differencie_differencie$Speicher_Cost + 1 - contrefactual_differencie_differencie$Speicher_cf_Volumes/(contrefactual_differencie_differencie$Binda_cf_Volumes + contrefactual_differencie_differencie$Guerra_cf_Volumes + contrefactual_differencie_differencie$Speicher_cf_Volumes - Market_Size)

plot(Bicilandia$Month, Bicilandia$Binda_Volume , type = "l" , col = "Blue", ylab = "Volume", mar = c(0,0,0,0), main = "Counterfactual Volumes with One-Model Approach and Differentiated Demand Functions",cex.main = 0.95, ylim = c(10000,65000))
lines(Bicilandia$Month, Bicilandia$Guerra_Volume, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Volume, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(contrefactual_differencie$Month,contrefactual_differencie$Binda_cf_Volumes, col = "blue", lty = 2)
lines(contrefactual_differencie$Month,contrefactual_differencie$Guerra_cf_Volumes, col = "green", lty = 2)
lines(contrefactual_differencie$Month,contrefactual_differencie$Speicher_cf_Volumes, col = "Red", lty = 2)
##
plot(Bicilandia$Month, Bicilandia$Binda_Price , type = "l" , col = "Blue",ylim = c(10,20), ylab = "Price", xlab = "", oma = c(0,0,0,0), main = "Counterfactual Prices with One-Model Approach and Differentiated Demand Functions", cex.main = 0.95)
lines(Bicilandia$Month, Bicilandia$Guerra_Price, type = "l", col = "Green")
lines(Bicilandia$Month, Bicilandia$Speicher_Price, type = "l", col = "Brown")
##
legend("topright", legend=c("Binda", "Guerra","Speicher"),
       col=c("Blue","Green","Brown"),lty = 1, cex=0.6)
lines(contrefactual_differencie$Month,contrefactual_differencie$Binda_cf_Price, col = "blue", lty = 2)
lines(contrefactual_differencie$Month,contrefactual_differencie$Guerra_cf_Price, col = "green", lty = 2)
lines(contrefactual_differencie$Month,contrefactual_differencie$Speicher_cf_Price, col = "Red", lty = 2)

######################################################################
## Calcul des dommages-intérêts modèle différencié par marque ##########
# r : taux d'intérêt annuel sur la période de collusion.  ##
Aujourdhui <- ymd("2017-01-01")


Damages_paidunits_Binda <- function(r) {
  return(sum((contrefactual_differencie$Binda_Price - contrefactual_differencie$Binda_cf_Price)*contrefactual_differencie$Binda_Volume*((1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))))
}
Damages_paidunits_Guerra <- function(r) {
  return(sum((contrefactual_differencie$Guerra_Price - contrefactual_differencie$Guerra_cf_Price)*contrefactual_differencie$Guerra_Volume*((1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))))
}
Damages_paidunits_Binda(0.015)
Damages_paidunits_Guerra(0.015)
Profits_Binda <- function(r) {
  sum((contrefactual_differencie$Binda_Price-contrefactual_differencie$Binda_Cost)*contrefactual_differencie$Binda_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Profits_Binda(0.015)

Harm_Binda <- function(r) {
  sum((contrefactual_differencie$Binda_cf_Volumes - contrefactual_differencie$Binda_Volume)*contrefactual_differencie$Binda_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Harm_Binda(0.015)
Harm_Guerra <- function(r) {
  sum((contrefactual_differencie$Guerra_cf_Volumes - contrefactual_differencie$Guerra_Volume)*contrefactual_differencie$Guerra_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Profits_Guerra <- function(r) {
  sum((contrefactual_differencie$Guerra_Price-contrefactual_differencie$Guerra_Cost)*contrefactual_differencie$Guerra_Volume*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Profits_Guerra(0.015)

Cf_Profits_Binda <- function(r) {
  sum((contrefactual_differencie$Binda_cf_Price - contrefactual_differencie$Binda_Cost)*contrefactual_differencie$Binda_cf_Volumes*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Cf_Profits_Binda(0.015)

Cf_Profits_Guerra <- function(r) {
  sum((contrefactual_differencie$Guerra_cf_Price - contrefactual_differencie$Guerra_Cost)*contrefactual_differencie$Guerra_cf_Volumes*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Cf_Profits_Guerra(0.015)

Harm_Speicher <- function(r) {
  sum((contrefactual_differencie$Speicher_cf_Volumes - contrefactual_differencie$Speicher_Volume)*contrefactual_differencie$Speicher_cf_Price*(1+r)^(as.numeric(Aujourdhui-contrefactual_differencie$Month)/365))
}
Harm_Speicher(0.015)