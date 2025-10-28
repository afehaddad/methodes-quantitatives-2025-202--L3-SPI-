# Introduction à R -
# Ce script est conçu pour une première séance pratique (lab session).
# Objectif : découvrir R, manipuler des objets, explorer un jeu de données,
# et produire des statistiques descriptives simples.

# 1. Les bases de R ----

# R peut effectuer des opérations arithmétiques simples
1 + 1
2 * 200
200 ^ 2
100 / 100

# En R, tout est un objet. On peut créer un objet et lui attribuer une valeur avec "<-"
mon_premier_objet <- "J'aime R"
mon_deuxieme_objet <- 1:10   # suite de 1 à 10
mon_troisieme_objet <- 1:5   # suite de 1 à 5

# On peut ensuite combiner ces objets
mon_deuxieme_objet + mon_troisieme_objet
mon_deuxieme_objet * mon_troisieme_objet

# Exemple : un objet numérique
x <- 5
y <- 10
z <- x + y
print(z)  # Affiche 15

# Exemple : un objet texte (caractère)
salutation <- "Bonjour, R !"
print(salutation)


# 2. Fonctions ----
# Une fonction prend des "arguments" en entrée et retourne un résultat.

carre <- function(n) {
  return(n^2) # élève un nombre au carré
}
carre(4)

# Fonctions déjà incluses dans R
log(10)    # logarithme népérien
sqrt(25)   # racine carrée
mean(c(1, 2, 3, 4, 5))  # moyenne d'un vecteur


# 3. Structures de données ----

# Vecteur : suite d’éléments du même type
partis <- c("Gauche", "Droite", "Centre", "Abstention")
votes <- c(100, 150, 80, 50)

# Liste : mélange de plusieurs types d’objets
ma_liste <- list(nom = "Alice", age = 30, scores = c(10, 12, 15))

# Matrice : tableau numérique à deux dimensions
mat <- matrix(1:9, nrow = 3)

# Data frame : tableau de données (l’outil central en science politique)
deputes <- data.frame(
  ID = 1:5,
  Nom = c("A", "B", "C", "D", "E"),
  Parti = c("Gauche", "Droite", "Centre", "Droite", "Gauche"),
  Mandats = c(2, 1, 3, 0, 1)
)
print(deputes)

# Accéder aux colonnes
deputes$Parti
deputes[1, ]   # première ligne
deputes[, "Mandats"]


# 4. Explorer un jeu de données existant ----
# Exemple : le jeu de données intégré "iris"
data("iris")
head(iris)     # premières lignes
str(iris)      # structure des variables
summary(iris)  # statistiques descriptives


# 5. Le tidyverse ----
# Le tidyverse est une collection de packages modernes pour manipuler et visualiser les données
install.packages("tidyverse")
library(tidyverse)

# Créons un petit jeu de données sur les circonscriptions électorales
circo <- tibble(
  Circonscription = c("Paris-1", "Paris-2", "Lyon-1", "Lyon-2", "Marseille-1"),
  Participation = c(62, 58, 65, 70, 55),  # taux de participation (%)
  Inscrits = c(50000, 60000, 45000, 48000, 52000),  # électeurs inscrits
  Femmes_élues = c(2, 1, 0, 1, 2)  # nombre de femmes élues
)

# Ajouter une nouvelle variable : nombre de votants
circo <- circo %>%
  mutate(Votants = Inscrits * Participation / 100)

# Trier les circonscriptions par participation
circo %>% arrange(desc(Participation))

# Calculer la participation moyenne
circo %>% summarise(Participation_moy = mean(Participation))


# 6. Visualisation avec ggplot2 ----
ggplot(circo, aes(x = Circonscription, y = Participation)) +
  geom_col(fill = "steelblue") +
  labs(title = "Participation électorale par circonscription",
       x = "Circonscription", y = "Taux de participation (%)") +
  theme_minimal()

# 7. Statistiques descriptives ----

# Exemple avec une variable quantitative
mean(circo$Participation)   # moyenne
median(circo$Participation) # médiane
sd(circo$Participation)     # écart-type

# Exemple avec une variable qualitative
table(circo$Femmes_élues)   # effectifs
prop.table(table(circo$Femmes_élues))  # proportions


# 8. Exercice pour les étudiant·e·s ----
# 1. Créer un nouveau jeu de données sur 5 partis politiques :
#    variables = Nom du parti, % des voix, Nombre de sièges
# 2. Calculer la moyenne du % des voix.
# 3. Représenter la répartition des sièges par parti avec un barplot.
