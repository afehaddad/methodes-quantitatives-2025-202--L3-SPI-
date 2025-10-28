###############################################################
# INTRODUCTION À R ET AUX DONNÉES "GUNS"
# Objectif du script :
# - Comprendre les types d’objets en R
# - Savoir importer un fichier CSV
# - Explorer et décrire les données
# - Réaliser des statistiques simples et des graphiques
# Jeu de données : Guns.csv (AER)
###############################################################


# ------------------------------------------------------------
# 1. PRÉPARATION DE L’ENVIRONNEMENT
# ------------------------------------------------------------

# On efface tout ce qui se trouve actuellement dans la mémoire de R
rm(list = ls())

# On vérifie dans quel dossier on se trouve actuellement :
getwd()   # “get working directory” : le dossier de travail

# Si besoin, on définit manuellement le dossier de travail
# (adapter le chemin à votre ordinateur)
# setwd("~/Documents/methodes-quantitatives-2025-2026/donnees")


# ------------------------------------------------------------
# 2. PREMIERS PAS : LES TYPES D’OBJETS EN R
# ------------------------------------------------------------

# R manipule différents types d’objets.
# On distingue deux grandes familles :
#   1. Les objets homogènes  → tous les éléments sont du même type
#   2. Les objets hétérogènes → plusieurs types peuvent coexister

# --- OBJETS HOMOGÈNES ---

# VECTEUR : contient des valeurs d’un même type (ex. que des nombres)
ages <- c(21, 25, 30, 28, 40)   # c() signifie “combine”
ages
mean(ages)   # moyenne
length(ages) # longueur du vecteur

# FACTEUR : variable qualitative (catégorielle)
sexe <- factor(c("Homme", "Femme", "Homme", "Femme"))
sexe
levels(sexe) # affiche les modalités

# MATRICE : tableau numérique homogène (même type dans toutes les cases)
mat <- matrix(1:9, nrow = 3, ncol = 3)  # 3 lignes, 3 colonnes
mat

# --- OBJETS HÉTÉROGÈNES ---

# LISTE : peut contenir des éléments de types différents
ma_liste <- list(nom = "Ahmed", age = 35, ville = "Fontainebleau")
ma_liste
ma_liste$age  # accéder à un élément avec $

# DATA FRAME : tableau de données (comme dans Excel)
# chaque colonne peut être d’un type différent
df <- data.frame(
  Nom = c("Alice", "Bob", "Charlie"),
  Âge = c(25, 30, 28),
  Sexe = c("F", "H", "H")
)
df

# Un data frame est donc un objet HÉTÉROGÈNE.
# Chaque colonne est un vecteur homogène, mais les colonnes peuvent différer.


# ------------------------------------------------------------
# 3. IMPORTER LE FICHIER “GUNS.CSV”
# ------------------------------------------------------------

# On suppose que le fichier “guns.csv” est dans le dossier de travail.
# L’argument stringsAsFactors=TRUE transforme automatiquement les textes en facteurs.

guns <- read.csv("guns.csv", stringsAsFactors = TRUE)

# Vérification : afficher les 6 premières lignes du jeu de données
head(guns)

# Afficher la structure du jeu de données (type de chaque colonne)
str(guns)

# Dimensions : combien de lignes et de colonnes ?
dim(guns)

# Noms de toutes les variables
names(guns)


# ------------------------------------------------------------
# 4. DESCRIPTION DU CONTENU
# ------------------------------------------------------------

# Ce fichier contient 1 173 observations et 13 variables :
# - Chaque ligne = un État américain pour une année donnée (1977–1999)
# - Exemples de variables :
#   state  : nom de l’État
#   year   : année
#   violent: taux de crimes violents (pour 100 000 habitants)
#   murder : taux de meurtres
#   law    : existence d’une loi “shall carry”
#   income : revenu par habitant (USD)
#   density: densité de population (en milliers / mile carré)


# ------------------------------------------------------------
# 5. STATISTIQUES DESCRIPTIVES DE BASE
# ------------------------------------------------------------

# Moyenne du taux de crimes violents
mean(guns$violent)
# → calcule la moyenne de la variable “violent”

# Médiane du taux de crimes violents
median(guns$violent)

# Écart-type
sd(guns$violent)

# Valeur minimale
min(guns$violent)

# Valeur maximale
max(guns$violent)

# Résumé global de toutes les variables numériques
summary(guns)
# → R affiche min, 1er quartile, médiane, moyenne, 3e quartile et max

# Table de fréquence pour une variable qualitative (loi)
table(guns$law)
# → compte le nombre d’États ayant ou non une loi “shall carry”


# ------------------------------------------------------------
# 6. EXPLORATION SIMPLE
# ------------------------------------------------------------

# Nombre d’observations par État
table(guns$state)

# Nombre d’années dans le jeu de données
table(guns$year)

# Combinaison État × Loi (utile pour voir qui adopte la loi)
table(guns$state, guns$law)


# ------------------------------------------------------------
# 7. AGRÉGATION : MOYENNES PAR GROUPE
# ------------------------------------------------------------

# La fonction aggregate() permet de calculer des statistiques par groupe.

# Exemple 1 : moyenne du taux de crimes violents selon la présence d’une loi
aggregate(violent ~ law, data = guns, FUN = mean)
# “violent ~ law” signifie : expliquer violent par law
# “data = guns” indique la base utilisée
# “FUN = mean” précise la fonction à appliquer (ici la moyenne)

# Exemple 2 : moyenne du taux de meurtres par loi
aggregate(murder ~ law, data = guns, FUN = mean)

# Exemple 3 : moyenne du taux de crimes violents par État
aggregate(violent ~ state, data = guns, FUN = mean)

# Exemple 4 : moyenne du taux de crimes violents par État et par Loi
aggregate(violent ~ state + law, data = guns, FUN = mean)

# Exemple 5 : plusieurs variables à la fois (violent, murder, robbery)
aggregate(cbind(violent, murder, robbery) ~ law, data = guns, FUN = mean)
# “cbind()” combine plusieurs variables ensemble


# ------------------------------------------------------------
# 8. VISUALISATION GRAPHIQUE (INTRODUCTION À ggplot2)
# ------------------------------------------------------------

# Si ggplot2 n’est pas installé, exécuter une seule fois :
# install.packages("ggplot2")

library(ggplot2)  # Charger le package

# Exemple 1 : histogramme du taux de crimes violents
ggplot(guns, aes(x = violent)) +                # aes() définit les axes
  geom_histogram(binwidth = 100,                # largeur des barres
                 fill = "steelblue",            # couleur de remplissage
                 color = "white") +             # bordures blanches
  labs(title = "Distribution du taux de crimes violents",
       x = "Taux de crimes violents (pour 100 000 habitants)",
       y = "Fréquence")

# Exemple 2 : évolution du taux de crimes violents dans un État (ex. Texas)
ggplot(subset(guns, state == "Texas"),          # on filtre seulement le Texas
       aes(x = as.numeric(as.character(year)),  # convertir year en numérique
           y = violent)) +                      # axe des y = taux violent
  geom_line(color = "darkred", size = 1) +      # tracé de la courbe
  labs(title = "Évolution du taux de crimes violents au Texas (1977–1999)",
       x = "Année", y = "Taux de crimes violents")


# ------------------------------------------------------------
# 9. AGRÉGATION ET VISUALISATION COMBINÉES
# ------------------------------------------------------------

# Calculer la moyenne du taux de crimes violents par État
crime_state <- aggregate(violent ~ state, data = guns, FUN = mean)

# Créer un graphique en barres (taux moyen par État)
ggplot(crime_state, aes(x = reorder(state, violent), y = violent)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +  # on inverse les axes pour plus de lisibilité
  labs(title = "Taux moyen de crimes violents par État (1977–1999)",
       x = "État", y = "Taux moyen de crimes violents")


# ------------------------------------------------------------
# 10. EXPORTER LES RÉSULTATS (OPTIONNEL)
# ------------------------------------------------------------

# Sauvegarder la table des moyennes par État dans un fichier CSV
write.csv(crime_state, "crime_moyen_par_etat.csv", row.names = FALSE)
# “row.names = FALSE” évite d’ajouter un numéro de ligne dans le fichier

# Le fichier est enregistré dans le répertoire de travail actuel.
# Vous pouvez le retrouver avec getwd().


###############################################################
# Fin du script d’introduction à R et au jeu de données "Guns"
###############################################################
