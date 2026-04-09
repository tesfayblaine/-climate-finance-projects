#Thème 5 : Étude de la substituabilité entre le pétrole et les énergies 
#renouvelables
#Sujet : Analyser la substituabilité entre le pétrole et les énergies 
#renouvelables dans les marchés énergétiques.
#Objectif : Appliquer des modèles de séries temporelles pour étudier dans quelle 
#mesure une hausse des prix du pétrole entraîne une substitution vers les 
#énergies renouvelables.

#Packages utilisés : 
library(readxl)
library(tseries)
library(forecast)
library(lmtest)
library(psych)

Brent <- read_excel("Brent.xlsx")
View(Brent)

head(Brent)
tail(Brent)
head(SPGlobal_Clean_Energy)
tail(SPGlobal_Clean_Energy)

SPGlobal_Clean_Energy <- read_excel("SPGlobal_Clean_Energy.xlsx")
View(SPGlobal_Clean_Energy)

str(SPGlobal_Clean_Energy$Dernier)

Brent$prix_brent <- Brent$Dernier
Brent$Dernier <- NULL

SPGlobal_Clean_Energy$prix_clean <- SPGlobal_Clean_Energy$Dernier
SPGlobal_Clean_Energy$Dernier <- NULL
View(SPGlobal_Clean_Energy)

prix_clean <- SPGlobal_Clean_Energy$prix_clean
prix_brent <- Brent$prix_brent

######################## STATISTIQUES DESCRIPTIVES ###########################

#générales
summary(Brent)
summary(SPGlobal_Clean_Energy)

#pour les données qu'on étudie

summary(prix_clean)
summary(prix_brent)

sd(prix_brent, na.rm = TRUE)
sd(prix_clean, na.rm = TRUE)

describe(cbind(prix_brent,prix_clean))

hist(prix_brent, prob = TRUE, breaks = 50, col = "hotpink2",
     main = "Distribution du prix du Brent", xlab = "Prix du Brent",
     border = "white")

d_brent <- density(prix_brent, na.rm = TRUE)
lines(d_brent, col = "black", lwd = 2)
legend("topright", legend = "Densité du prix du Brent", lty = 1, lwd = 2,
       col = "black",cex = 0.7)


hist(prix_clean, prob = TRUE, breaks = 50, col = "mediumaquamarine",
     main = "Distribution du prix de ENR", 
     xlab = "Prix des énergies renouvelables",  border = "white")

d_clean <- density(prix_clean, na.rm = TRUE)
lines(d_clean, col = "black", lwd = 2)

legend("topright", legend = "Densité du prix_clean", lty = 1, lwd = 2,
       col = "black",cex = 0.7)

######################## Modèle ARMA ############################

#Création des séries temporelles, des rendemments et visualisation. 
ts_brent <-ts(Brent$prix_brent, start =c(2007,3), frequency=12)
ts_enr <-ts(SPGlobal_Clean_Energy$prix_clean, start =c(2007,3), frequency=12)

plot(ts_brent, main="Série brute : Prix du Brent", ylab="USD", col="hotpink3")
plot(ts_enr, main="Série brute : Prix Clean Energy", ylab="Indice", 
     col="olivedrab4")

summary(ts_brent)
summary(ts_enr)

R_brent = diff(log(ts_brent))
R_enr=diff(log(ts_enr))

(cor(R_brent, R_enr))
describe(cbind(R_brent, R_enr))

plot(R_brent, main ="Rendement du pétrole", col = "mediumpurple3")
plot(R_enr, main ="Rendement des ENR", col ="lightcoral")

#Quelques graphiques sur les rendements du Brent et de ENR
# Histogramme et densité R_brent
hist(R_brent, prob = TRUE, breaks = 50, col = "rosybrown1", 
     main = "Densité des rendements Brent", xlab = "Rendements", 
     border = "white")

d_brent <- density(R_brent)
lines(d_brent, col = "grey27", lwd = 2)

legend("topright", legend = "Densité Brent", lty = 1, lwd = 2, 
       col = "grey27", cex = 0.7)

# Histogramme et densité R_enr
  hist(R_enr, prob = TRUE, breaks = 50, col = "plum", 
       main = "Densité des rendements des énergies renouvelables", 
       xlab = "Rendements", border = "white")
  
d_enr <- density(R_enr)
lines(d_enr, col = "grey27", lwd = 2)

legend("topright", legend = "Densité ENR", lty = 1, lwd = 2, 
       col = "grey27", cex = 0.7)

#### Boxplot supplémentaire : (Comparaison R_enr et R_brent)
boxplot(R_enr, R_brent,
        names = c("Clean Energy", "Brent"),
        main = "Comparaison des rendements logarithmiques",
        ylab = "Rendements",
        col = c("steelblue3", "orange"))

#Le boxplot met en évidence une dispersion plus élevée des rendements 
#du Clean Energy par rapport au Brent, traduisant une volatilité plus forte.

########## A) pour le brent #################

adf.test(R_brent) 
pp.test(R_brent)
kpss.test(R_brent)
#hypothèses de stationnarité vérifiée. 
#Passons au modèle ARMA : 

acf(R_brent)
pacf(R_brent)

#On a p et q entre 0 et 1
brent.arima100 <- arima(x = R_brent, order = c(1,0,0))  # AR(1)
brent.arima001 <- arima(x = R_brent, order = c(0,0,1))  # MA(1)
brent.arima101 <- arima(x = R_brent, order = c(1,0,1))  # ARMA(1,1)

AIC(brent.arima001,brent.arima100,brent.arima101)

tsdiag(brent.arima100)  # AR(1)
tsdiag(brent.arima001)  # MA(1)
tsdiag(brent.arima101)  # ARMA(1,1)

Box.test(residuals(brent.arima100), lag=12, type="Ljung-Box")
Box.test(residuals(brent.arima001), lag=12, type="Ljung-Box")
Box.test(residuals(brent.arima101), lag=12, type="Ljung-Box")

##Le principe de parcimonie conduit à retenir le modèle AR

#Le modèle retenu est un ARIMA(1,0,0),(donc AR(1))car il minimise l’AIC et les 
#tests sur les résidus (Ljung-Box) ne détectent pas d’autocorrélation.

########## B) pour les énergies renouvelables #########

#Test de stationnarité
adf.test(R_enr)
pp.test(R_enr)
kpss.test(R_enr)
#La série est bien stationnaire. 

#La série est stationnaire. 

acf(R_enr) #p entre 0 et 1
pacf(R_enr) #q entre 0,1,2. 

enr.arima100 <- arima(x = R_enr, order = c(1,0,0))  # AR(1)
enr.arima001 <- arima(x = R_enr, order = c(0,0,1))  # MA(1)
enr.arima101 <- arima(x = R_enr, order = c(1,0,1))  # ARMA(1,1)
enr.arima002 <- arima(x = R_enr, order = c(0,0,2))  # MA(2)
enr.arima102 <- arima(x = R_enr, order = c(1,0,2))  # ARMA(1,2)

AIC(enr.arima100,enr.arima001,enr.arima101,enr.arima002,enr.arima102)
#Le plus petit AIC est pour le modèle MA(1), -437,5505

tsdiag(enr.arima100)  
tsdiag(enr.arima001)  

Box.test(residuals(enr.arima100), lag=12, type="Ljung-Box")
Box.test(residuals(enr.arima001), lag=12, type="Ljung-Box")

#Donc, le modèle retenu pour Clean Energy est ARIMA (0,0,1), donc MA(1).

#Le modèle ARIMA(0,0,1) est retenu car il minimise le critère AIC et l’analyse 
#des résidus (ACF et test de Ljung-Box) ne met pas en évidence d’autocorrélation
#résiduelle. Les résidus peuvent ainsi être assimilés à un bruit blanc.


############################## ARIMAX ###############################
length(R_brent) #225
length(R_enr) #225
#les séries sont bien de même longueur. 

# Affichage des résultats pour comparer avec R_enr et R_brent : étudier s'il 
#y a une interdépendance entre ces deux variables. 

fit_arimax <- auto.arima(R_enr, xreg = R_brent, seasonal = FALSE)
summary(fit_arimax)
coeftest(fit_arimax)
checkresiduals(fit_arimax)

