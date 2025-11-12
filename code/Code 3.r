# ===============================================================
# ANALYSE DU JEU DE DONNÉES TITANIC
# ---------------------------------------------------------------
# Objectifs :
# 1. Analyse univariée
# 2. Analyse bivariée
# 3. Test du Chi²
# 4. V de Cramer
# 5. Odds Ratio
# ===============================================================

# --- Packages utiles ---
library(ggplot2)
library(vcd)          # Pour assocstats() : V de Cramer, Chi², etc.
library(epitools)     # Pour oddsratio()

# --- Jeu de données ---
data("Titanic")
titanic <- as.data.frame(Titanic)

# --------------------------------------------------------------
# 1. ANALYSE UNIVARIÉE
# --------------------------------------------------------------

# Répartition par classe
table_class <- tapply(titanic$Freq, titanic$Class, sum)
table_class
round(100 * table_class / sum(table_class), 1)

# Répartition par sexe
table_sex <- tapply(titanic$Freq, titanic$Sex, sum)
table_sex
round(100 * table_sex / sum(table_sex), 1)

# Graphiques (ggplot2)
ggplot(titanic, aes(x = Class, weight = Freq, fill = Class)) +
  geom_bar() +
  labs(title = "Répartition des passagers par classe",
       x = "Classe", y = "Nombre de passagers") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(titanic, aes(x = Sex, weight = Freq, fill = Sex)) +
  geom_bar() +
  labs(title = "Répartition des passagers par sexe",
       x = "Sexe", y = "Nombre de passagers") +
  theme_minimal() +
  theme(legend.position = "none")

# --------------------------------------------------------------
# 2. ANALYSE BIVARIÉE : SURVIE SELON LE SEXE
# --------------------------------------------------------------

tab_sex_surv <- xtabs(Freq ~ Sex + Survived, data = titanic)
tab_sex_surv

# Proportions conditionnelles
round(prop.table(tab_sex_surv, 1), 2)

# Graphique
ggplot(as.data.frame(tab_sex_surv), aes(x = Sex, y = Freq, fill = Survived)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Taux de survie selon le sexe",
       y = "Proportion", x = "Sexe") +
  theme_minimal()

# --------------------------------------------------------------
# 3. TEST DU CHI² ET V DE CRAMER
# --------------------------------------------------------------

# Test du Chi²
chisq.test(tab_sex_surv)

# Statistiques d’association (V de Cramer inclus)
assocstats(tab_sex_surv)

# Le V de Cramer est indiqué à la ligne :
# "Cramer's V" → mesure la force de l'association (0 = aucun lien, 1 = fort lien)

# --------------------------------------------------------------
# 4. ODDS RATIO (tableau 2x2)
# --------------------------------------------------------------

# Calcul de l’odds ratio avec IC à 95 %
oddsratio(tab_sex_surv, method = "wald")

# Interprétation :
# OR > 1 → les femmes ont plus de chances de survivre
# OR < 1 → les hommes ont plus de chances de survivre

# --------------------------------------------------------------
# 5. AUTRE EXEMPLE : SURVIE SELON LA CLASSE
# --------------------------------------------------------------

tab_class_surv <- xtabs(Freq ~ Class + Survived, data = titanic)
tab_class_surv

# Test du Chi² et V de Cramer
chisq.test(tab_class_surv)
assocstats(tab_class_surv)

# Graphique
ggplot(as.data.frame(tab_class_surv), aes(x = Class, y = Freq, fill = Survived)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Taux de survie selon la classe",
       x = "Classe", y = "Proportion") +
  theme_minimal()
