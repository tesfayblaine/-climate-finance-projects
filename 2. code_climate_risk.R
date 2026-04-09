#### PROJET : ANALYSE DES REFUS DE PRÊTS ET RISQUE CLIMATIQUE ####
############ Réalisé par TESFAY Blaine et TOUAHRIA Irène ###########

library(readxl)
library(ggplot2)
library(scales)
library(moments)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(sandwich)
library(lmtest)
library(zoo)
donne_es_2025 <- read_excel("données_2025.xlsx")
View(donne_es_2025)
donnee<- donne_es_2025
View(donnee)
######## 1.1) Présentation du jeu de données et préparation des variables ########
head(donnee)
tail(donnee)
str(donnee)
dim(donnee) #n = 12 074, p=14
names(donnee)
# Création de la variable explicative : race_black
donnee$race_black <- ifelse(donnee$race == "Black", 1, 0)
donnee$race_black <- factor(donnee$race_black,
                            levels = c(0, 1),
                            labels = c("Autre", "Black"))
# Transformation des variables à expliquer (Refus) en facteurs
donnee$denial_reason_collateral <- factor(donnee$denial_reason_collateral,
                                          levels = c(0, 1),
                                          labels = c("Non", "Oui"))
donnee$denial_reason_debt_income <- factor(donnee$denial_reason_debt_income,
                                           levels = c(0, 1),
                                           labels = c("Non", "Oui"))
donnee$denial_reason_insuff_cash <- factor(donnee$denial_reason_insuff_cash,
                                           levels = c(0, 1),
                                           labels = c("Non", "Oui"))
26
# Transformation du risque climatique
donnee$climate_hazard <- factor(donnee$climate_hazard,
                                levels = c(0, 1), labels = c("Non", "Oui"))
########## 1.2) Statistiques descriptives ##########
## A) Variables numériques
summary(donnee[, c("income", "lag_sp_population", "lag_sp_unemployment")])
###1.2) Statistiques descriptives
##A) Variables numériques : moyenne, médiane, min, max, écart-type
summary(donnee)
# Revenu
summary(donnee$income)
sd(donnee$income, na.rm = TRUE)
# Population (retardée)
summary(donnee$lag_sp_population)
sd(donnee$lag_sp_population, na.rm = TRUE)
# Chômage (retardé)
summary(donnee$lag_sp_unemployment)
sd(donnee$lag_sp_unemployment, na.rm = TRUE)
# Revenu médian des ménages (retardé)
summary(donnee$lag_sp_medianhhincome)
sd(donnee$lag_sp_medianhhincome, na.rm = TRUE)
##B) Variables binaires, catégorielles : fréquences et proportions
# Part des emprunteurs noirs (race_black)
table(donnee$race_black)
round(100 * prop.table(table(donnee$race_black)), 2)
#Fréquence des catastrophes naturelles
table(donnee$climate_hazard)
round(100 * prop.table(table(donnee$climate_hazard)), 2)
##C) Variables à expliquer
# Refus pour garantie insuffisante
table(donnee$denial_reason_collateral)
round(100 * prop.table(table(donnee$denial_reason_collateral)), 2)
# Refus pour ratio dette / revenu
table(donnee$denial_reason_debt_income)
round(100 * prop.table(table(donnee$denial_reason_debt_income)), 2)
27
# Refus pour liquidités insuffisantes
table(donnee$denial_reason_insuff_cash)
round(100 * prop.table(table(donnee$denial_reason_insuff_cash)), 2)
##D) Taux de refus selon le motif (croisement simple)
#Refus pour garantie selon la race
table(donnee$denial_reason_collateral, donnee$race_black)
round(100 * prop.table(table(donnee$denial_reason_collateral,
                             donnee$race_black), margin = 2), 2)
#Refus pour garantie selon le risque climatique
table(donnee$denial_reason_collateral, donnee$climate_hazard)
round(100 * prop.table(table(donnee$denial_reason_collateral,
                             donnee$climate_hazard), margin = 2), 2)
#Refus pour dette/revenu selon la race
table(donnee$denial_reason_debt_income, donnee$race_black)
round(100 * prop.table(table(donnee$denial_reason_debt_income,
                             donnee$race_black), margin = 2), 2)
# Refus pour dette/revenu selon le risque climatique
table(donnee$denial_reason_debt_income, donnee$climate_hazard)
round(100 * prop.table(table(donnee$denial_reason_debt_income,
                             donnee$climate_hazard), margin = 2), 2)
# Refus pour liquidités insuffisantes selon la race
table(donnee$denial_reason_insuff_cash, donnee$race_black)
round(100 * prop.table(table(donnee$denial_reason_insuff_cash,
                             donnee$race_black), margin = 2), 2)
# Refus pour liquidités insuffisantes selon le risque climatique
table(donnee$denial_reason_insuff_cash, donnee$climate_hazard)
round(100 * prop.table(table(donnee$denial_reason_insuff_cash,
                             donnee$climate_hazard), margin = 2), 2)
##E) Interaction descriptive : race_black × climate_hazard (4 groupes)
donnee$race_climat <- interaction(donnee$race_black, donnee$climate_hazard, sep = " | ")
table(donnee$race_climat)
round(100 * prop.table(table(donnee$race_climat)), 2)
# Taux de refus par groupe (4 modalités) : Garantie
table(donnee$denial_reason_collateral, donnee$race_climat)
round(100 * prop.table(table(donnee$denial_reason_collateral,
                             donnee$race_climat), margin = 2), 2)
# Dette/Revenu
table(donnee$denial_reason_debt_income, donnee$race_climat)
28
round(100 * prop.table(table(donnee$denial_reason_debt_income,
                             donnee$race_climat), margin = 2), 2)
# Liquidités insuffisantes
table(donnee$denial_reason_insuff_cash, donnee$race_climat)
round(100 * prop.table(table(donnee$denial_reason_insuff_cash,
                             donnee$race_climat), margin = 2), 2)
######## 1.3) Dataviz ########
######## 1.3.1) Dataviz : Analyse Univariée ########
#### A) ANALYSE DES VARIABLES NUMÉRIQUES
# 1) Revenu et moments
# Style Sh.R : On affiche les moments avant de tracer
skewness(donnee$income, na.rm = TRUE) # 6.48, revenu asymétrique.
kurtosis(donnee$income, na.rm = TRUE) # 102.8
# Histogramme du revenu (On utilise le log pour corriger l'asymétrie de 6.48)
hist(log(donnee$income[donnee$income > 0]), freq = FALSE,
     main = "Histogramme du log-revenu des emprunteurs",
     xlab = "log(Income)", col = "lightpink", border = "white")
lines(density(log(donnee$income[donnee$income > 0]), na.rm = TRUE),
      col = "darkmagenta", lwd = 2)
legend("topright", legend = c("Densité"), col = "darkmagenta", lwd = 2, bty = "n")
# 2) Chômage régional
hist(donnee$lag_sp_unemployment, freq = FALSE,
     main = "Histogramme du taux de chômage régional",
     xlab = "Taux de chômage", col = "seashell2", border = "white")
lines(density(donnee$lag_sp_unemployment, na.rm = TRUE), col = "blue", lwd = 2)
legend("topright", legend = c("Densité"), col = "blue", lwd = 2, bty = "n")
#### B) ANALYSE DES VARIABLES QUALITATIVES (Effectifs)
# Barplots des motifs de refus
barplot(table(donnee$denial_reason_collateral),
        col = "coral",
        main = "Diagramme en barres du refus pour garantie",
        xlab = "Réponse",
        ylab = "Nombre de dossiers")
barplot(table(donnee$denial_reason_debt_income),
        col = "skyblue",
        main = "Diagramme en barres du refus pour ratio dette / revenu",
        xlab = "Réponse",
        ylab = "Nombre de dossiers")
29
# Barplots des groupes
barplot(table(donnee$race_black),
        col = "plum",
        main = "Diagramme en barres de la répartition des emprunteurs par groupe racial",
        xlab = "Groupe",
        ylab = "Nombre d'emprunteurs")
####### 1.3.2) Dataviz : Analyse Bivariée (Explication des refus) #######
#### A) Boxplots (Lien Revenu et Profil)
# Revenu selon la race (avec log pour la lisibilité)
boxplot(log(income) ~ race_black,
        data = donnee[donnee$income > 0, ],
        vertical = TRUE,
        main = "Boîte à moustache du selon le groupe racial",
        xlab = "Groupe Racial",
        ylab = "Revenu",
        col = c("azure", "dodgerblue"))
var.test(log(income) ~ race_black, data = donnee[donnee$income > 0, ])
t.test(log(income) ~ race_black, data = donnee[donnee$income > 0, ],
       var.equal=FALSE)
# Revenu selon le climat
boxplot(log(income) ~ climate_hazard, data = donnee[donnee$income > 0, ],
        vertical = TRUE, col = c("green", "pink"),
        main = "Boîte à moustaches du log-revenu selon l'exposition climatique",
        xlab = "Exposition au risque climatique",
        ylab = "log(Revenu)")
var.test(log(income) ~ climate_hazard, data = donnee[donnee$income > 0, ])
t.test(log(income) ~ climate_hazard, data = donnee[donnee$income > 0, ],
       var.equal = FALSE)
#### B) GGPlots (Proportions de refus)
#### B) Proportions de refus ####
# 1) Refus Garantie selon la Race
round(100 * prop.table(table(donnee$denial_reason_collateral, donnee$race_black), 2), 2)
ggplot(data = na.omit(donnee[, c("race_black", "denial_reason_collateral")]),
       aes(x = race_black, fill = denial_reason_collateral)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("azure", "dodgerblue"), name = "Refus") +
  labs(
    title = "Proportion de refus pour garantie selon la race",
    subtitle = "Diagramme empilé à 100% (part de 'Oui' vs 'Non')",
    30
    x = "Groupe racial",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
chisq.test(donnee$denial_reason_collateral, donnee$race_black)
# 2) Refus Garantie selon le risque climatique
round(100 * prop.table(table(donnee$denial_reason_collateral, donnee$climate_hazard), 2), 2)
ggplot(data = na.omit(donnee[, c("climate_hazard", "denial_reason_collateral")]),
       aes(x = climate_hazard, fill = denial_reason_collateral)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("white", "darkred"), name = "Refus") +
  labs(
    title = "Proportion de refus pour garantie selon le risque climatique",
    subtitle = "Comparaison entre zones exposées et non exposées",
    x = "Exposition au risque climatique",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
chisq.test(donnee$denial_reason_collateral, donnee$climate_hazard)
# 3) Refus Dette/Revenu selon la Race
round(100 * prop.table(table(donnee$denial_reason_debt_income, donnee$race_black), 2), 2)
ggplot(data = na.omit(donnee[, c("race_black", "denial_reason_debt_income")]),
       aes(x = race_black, fill = denial_reason_debt_income)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("ivory", "goldenrod3"), name = "Refus") +
  labs(
    title = "Proportion de refus pour ratio dette / revenu selon la race",
    subtitle = "Diagramme empilé à 100% (part de 'Oui' vs 'Non')",
    x = "Groupe racial",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
# 4) Refus Dette/Revenu selon le risque climatique
round(100 * prop.table(table(donnee$denial_reason_debt_income, donnee$climate_hazard), 2), 2)
ggplot(data = na.omit(donnee[, c("climate_hazard", "denial_reason_debt_income")]),
       aes(x = climate_hazard, fill = denial_reason_debt_income)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("white", "darkorange3"), name = "Refus") +
  labs(
    31
    title = "Proportion de refus pour ratio dette / revenu selon le risque climatique",
    subtitle = "Comparaison entre zones exposées et non exposées",
    x = "Exposition au risque climatique",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
# 5) Refus Liquidités insuffisantes selon la Race
round(100 * prop.table(table(donnee$denial_reason_insuff_cash, donnee$race_black), 2), 2)
ggplot(data = na.omit(donnee[, c("race_black", "denial_reason_insuff_cash")]),
       aes(x = race_black, fill = denial_reason_insuff_cash)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("mintcream", "mediumseagreen"), name = "Refus") +
  labs(
    title = "Proportion de refus pour liquidités insuffisantes selon la race",
    subtitle = "Graphique complémentaire ",
    x = "Groupe racial",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
# 6) Refus Liquidités insuffisantes selon le risque climatique
round(100 * prop.table(table(donnee$denial_reason_insuff_cash, donnee$climate_hazard), 2), 2)
ggplot(data = na.omit(donnee[, c("climate_hazard", "denial_reason_insuff_cash")]),
       aes(x = climate_hazard, fill = denial_reason_insuff_cash)) +
  geom_bar(position = "fill", width = 0.6, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("white", "forestgreen"), name = "Refus") +
  labs(
    title = "Proportion de refus pour liquidités insuffisantes selon le risque climatique",
    subtitle = "Graphique complémentaire ",
    x = "Exposition au risque climatique",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
#### C) Relation entre variables numériques ####
ggplot(data = donnee[donnee$income > 0, ],
       aes(x = lag_sp_unemployment, y = log(income))) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkmagenta", se = FALSE) +
  labs(
    title = "Nuage de points du log-revenu et du chômage régional",
    subtitle = "Visualisation d'une relation potentielle entre contexte régional et niveau de revenu",
    x = "Taux de chômage régional (lag)",
    y = "log(Revenu)"
    32
  ) +
  theme_classic()
#### D) Interaction race_black × climate_hazard (4 groupes) ####
# 1) Garantie
ggplot(data = na.omit(donnee[, c("race_climat", "denial_reason_collateral")]),
       aes(x = race_climat, fill = denial_reason_collateral)) +
  geom_bar(position = "fill", width = 0.7, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("ghostwhite", "purple4"), name = "Refus") +
  labs(
    title = "Refus de prêt pour garantie insuffisante selon le profil des emprunteurs",
    subtitle = "Comparaison des taux de refus selon la race et l’exposition au risque climatique",
    x = "Profil des emprunteurs",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
# 2) Dette/Revenu
ggplot(data = na.omit(donnee[, c("race_climat", "denial_reason_debt_income")]),
       aes(x = race_climat, fill = denial_reason_debt_income)) +
  geom_bar(position = "fill", width = 0.7, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("snow", "firebrick3"), name = "Refus") +
  labs(
    title = "Refus de prêt pour ratio dette / revenu selon le profil des emprunteurs",
    subtitle = "Effet combiné de la race et de l’exposition au risque climatique",
    x = "Profil des emprunteurs",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
chisq.test(donnee$denial_reason_collateral, donnee$race_climat)
# 3) Liquidités insuffisantes
ggplot(data = na.omit(donnee[, c("race_climat", "denial_reason_insuff_cash")]),
       aes(x = race_climat, fill = denial_reason_insuff_cash)) +
  geom_bar(position = "fill", width = 0.7, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("white", "royalblue4"), name = "Refus") +
  labs(
    title = "Refus de prêt pour liquidités insuffisantes selon le profil des emprunteurs",
    subtitle = "Analyse descriptive complémentaire",
    x = "Profil des emprunteurs",
    y = "Part des dossiers (%)"
  ) +
  theme_classic()
33
###### 1.4) Analyse de données : Analyse Factorielle Mixte (FAMD)
# Sélection des variables pertinentes dans notre étude
colonnes_famd <- c("race_black", "climate_hazard", "income",
                   "lag_sp_unemployment", "lag_sp_population",
                   "denial_reason_collateral", "denial_reason_debt_income",
                   "denial_reason_insuff_cash",
                   "num_financial_institutions")
donnee_famd <- donnee[, colonnes_famd]
donnee_famd$income <- as.numeric(donnee_famd$income)
donnee_famd$income[is.na(donnee_famd$income) | donnee_famd$income <= 0] <- NA
donnee_famd$income <- log(donnee_famd$income)
imp <- imputeFAMD(donnee_famd, ncp = 2) #fonction de R à partir du package missMDA
donnee_famd_imp <- imp$completeObs
# Exécution de l'analyse
res_famd <- FAMD(donnee_famd_imp, graph = FALSE)
fviz_screeplot(res_famd, addlabels = TRUE, main = "Inertie par dimension")
summary(res_famd) #pour voir les valeurs importantes avec cos2
res_famd$eig
get_eigenvalue(res_famd)
dimdesc(res_famd)
# Cercle des corrélations des variables
# Visualise quelles variables structurent les axes.
fviz_famd_var(res_famd, repel = TRUE, col.var = "contrib")
# Cercle des corrélations : variables quantitatives uniquement
fviz_famd_var(res_famd,
              "quanti.var",
              repel = TRUE,
              col.var = "steelblue")
# Carte des modalités
# Rechercher la proximité entre "Black", "Oui" (Climat) et "Oui" (Refus).
fviz_famd_var(res_famd, "quali.var", repel = TRUE, col.var = "black")
# Ellipses de confiance (Segmentation par Race)
# Vérifie si les dossiers "Black" se situent dans une zone spécifique du plan.
fviz_famd_ind(res_famd, habillage = "race_black",
              addEllipses = TRUE, ellipse.level = 0.95,
              label = "none", title = "Différenciation des profils par Race")
# Ellipses de confiance (Segmentation par Risque Climatique)
# Identifie si l'exposition climatique crée des profils d'emprunteurs distincts.
fviz_famd_ind(res_famd, habillage = "climate_hazard",
              addEllipses = TRUE, ellipse.level = 0.95,
              label = "none", title = "Différenciation par Risque Climatique")
34
# Contributions des variables (Axe 1)
fviz_contrib(res_famd, choice = "var", axes = 1, top = 10)
# Contributions des variables (Axe 2)
fviz_contrib(res_famd, choice = "var", axes = 2, top = 10, fill = "indianred")
###### 1.5) Analyse économétrique
donnee_reg <- subset(donnee, !is.na(income) & income > 0)
#pour enlever l'asymétrie observée avant
# Recodage des motifs de refus en 0/1 (Modèle Linéaire de Probabilité)
donnee_reg$Y_garantie <- ifelse(donnee_reg$denial_reason_collateral == "Oui", 1, 0)
donnee_reg$Y_drdi <- ifelse(donnee_reg$denial_reason_debt_income == "Oui", 1, 0)
donnee_reg$Y_insuff_cash <- ifelse(donnee_reg$denial_reason_insuff_cash == "Oui", 1, 0)
reg1 <- lm(Y_garantie ~ race_black * climate_hazard + log(income) +
             lag_sp_unemployment + num_financial_institutions, data = donnee_reg)
summary(reg1)
plot(reg1, which = 1) #residus va y_exp
plot(reg1, which = 2) #QQplot for normality
plot(reg1, which = 3) #resid.std and y_exp
plot(reg1, which = 4) #cook's distance
AIC(reg1)
# Identification de l'observation ayant le plus fort levier
index_influent1 <- which.max(cooks.distance(reg2))
View(donnee_reg[index_influent1, ])
index_influent1
# Régression 2 : Refus pour cause de ratio dette/revenu élevé
reg2 <- lm(Y_drdi ~ race_black * climate_hazard + log(income) +
             lag_sp_unemployment, data = donnee_reg)
summary(reg2)
plot(reg2, which = 1) #residus va y_exp
plot(reg2, which = 2) #QQplot for normality
plot(reg2, which = 3) #resid.std and y_exp
plot(reg2, which = 4) #cook's distance
AIC(reg2)
# Identification de l'observation ayant le plus fort levier
index_influent2 <- which.max(cooks.distance(reg2))
View(donnee_reg[index_influent2, ])
index_influent2
35
# Régression 3 : Refus pour cause de liquidités insuffisantes
reg3 <- lm(Y_insuff_cash ~ climate_hazard + num_financial_institutions +
             race_black + log(income), data = donnee_reg)
summary(reg3)
plot(reg3, which = 1) #residus va y_exp
plot(reg3, which = 2) #QQplot for normality
plot(reg3, which = 3) #resid.std and y_exp
plot(reg3, which = 4) #cook's distance
AIC(reg3)
# Identification de l'observation ayant le plus fort levier
index_influent3 <- which.max(cooks.distance(reg3))
View(donnee_reg[index_influent3, ])
index_influent3