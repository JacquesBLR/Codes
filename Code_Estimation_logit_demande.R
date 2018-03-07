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
Market_Size <- max(Bicilandia$Total_Volume) + 5
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

## On filtre sur la période sans collusion
logitm <- rbind(logitBinda,logitGuerra,logitSpeicher)
logitm <- subset(logitm, Month < ymd("2006-12-01"))
logitm <- subset(logitm, Month > ymd("2005-01-01"))
##
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
## On passe dans une phase "Cournot"
inverse <- function(f,y) {
  fonction_racine <- function(x) {
    return(f(x)-y)
  }
  return(uniroot(fonction_racine,c(0.01,12))$root[1])
}
exponentielle <- function(x) {
  return(exp(x))
}

## On récupère maintenant les données observées sur la période de collusion
## On filtre sur la période sans collusion
logitm <- rbind(logitBinda,logitGuerra,logitSpeicher)
logitm <- subset(logitm, Month < ymd("2012-01-01"))
logitm <- subset(logitm, Month > ymd("2007-12-01"))
##
modelelogit <- ivreg(ln_Share ~ Price | Cost , data = logitm)
## Construction du modèle contrefactuel
contrefactual <- Bicilandia[37:84,]
contrefactual$Binda_Residuals <- modelelogit$residuals[1:48]
contrefactual$Guerra_Residuals <- modelelogit$residuals[49:96]
contrefactual$Speicher_Residuals <- modelelogit$residuals[97:144]
alpha <- modelelogit$coefficients[[1]]
mu <- -modelelogit$coefficients[[2]]
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
  return(uniroot(fonction_racine,c(1e-8,1))$root[1])
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
# g_bertrand <- function(s) {
#   return(s + inverse_f_bertrand(alpha + log(s) + contrefactual$Binda_Residuals[1] - mu*contrefactual$Binda_Cost[1]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Guerra_Residuals[1] - mu*contrefactual$Guerra_Cost[1]) + inverse_f_bertrand(alpha + log(s) + contrefactual$Speicher_Residuals[1] - mu*contrefactual$Speicher_Cost[1]))
# }
# inverse2(g_bertrand,1)
