# Bloc 1 — Création et inspection des données -------------------------------

# 1) Créer les vecteurs
heures <- c(2, 4, 6, 8, 10)
notes  <- c(55, 65, 70, 80, 85)

# 2) Construire le data frame
data <- data.frame(heures = heures, notes = notes)
data[4,1]

str(data)
head(data, 10)

summary(data)

plot(
  x = data$heures, y = data$notes,
  xlab = "Heures d'étude", ylab = "Notes",
  pch = 19, col = "steelblue",
  main = "Relation entre heures d'étude et notes"
)
grid()


# 1) Ajuster le modèle linéaire
modele <- lm(notes ~ heures, data = data)
modele
summary(modele)

# Bloc 3 — Visualisation du modèle ------------------------------------------

# 1) Nuage de points
plot(
  x = data$heures, y = data$notes,
  xlab = "Heures d'étude", ylab = "Notes",
  pch = 19, col = "steelblue",
  main = "Relation entre heures d'étude et notes"
)

# 2) Ajout de la droite de régression
abline(modele, col = "red", lwd = 2)
grid()

# Bloc 4 — Prédictions et évaluation ----------------------------------------

# 1) Prédictions sur les données existantes
predictions <- predict(modele, newdata = data)
predictions

# 2) Comparer valeurs réelles vs prédictions
comparaison <- data.frame(
  Heures = data$heures,
  Notes_reelles = data$notes,
  Notes_predites = round(predictions, 2),
  Residus = round(data$notes - predictions, 2)
)
comparaison

# 3) Calculer l'erreur quadratique moyenne (RMSE)
rmse <- sqrt(mean((data$notes - predictions)^2))
rmse

# 4) Vérifier le R² directement
summary(modele)$r.squared

