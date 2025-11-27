context_anova <- function(data, reponse, facteur1, facteur2) {
  # Vérifiactions des Colonnes
  colonnes <- c(reponse, facteur1, facteur2)

  # Vérification des NA
  if (any(sapply(data[colonnes], function(x) any(is.na(x))))) {
    stop("Les colonnes sélectionnées contiennent des NA. Veuillez les supprimer ou les imputer avant de continuer.")
  }

  # Vérification que la réponse est numérique
  if (!is.numeric(data[[reponse]]))
    stop("La variable réponse doit être numérique.")

  # Vérification qu'au moins un facteur est catégoriel et conversion en factor
  for (f in c(facteur1, facteur2)) {
    n_modalites <- length(unique(data[[f]]))
    if (!is.numeric(data[[f]]) || (is.numeric(data[[f]]) && n_modalites > 1)) {
      data[[f]] <- as.factor(data[[f]])
    }
  }

  # Construction de la formule
  formule <- as.formula(paste(reponse, "~", facteur1, "*", facteur2))

  # Ajustement du modèle ANOVA
  modele <- aov(formule, data = data)

  # Résidus
  residus <- residuals(modele)

  # Tests de normalité et d'homogénéité des variances
  shapiro <- try(shapiro.test(residus), silent = TRUE)

  # Test de Levene
  if (!requireNamespace("car", quietly = TRUE)) {
    install.packages("car")
    library(car)
  }
  levene <- try(car::leveneTest(formule, data = data, center = median), silent = TRUE)


  # Sortie
  sortie <- list(
    model = modele,
    residuals = residus,
    shapiro = shapiro,
    levene = levene,
    data = data,
    facteur1 = facteur1,
    facteur2 = facteur2,
    reponse = reponse
  )

  class(sortie) <- "anova2"
  return(sortie)
}

print.anova2 <- function(obj, ...) {
  cat("Le résumé de l'analyse de la variance est :\n")
  print(summary(obj$model))

  cat("\nLe test de normalité des résidus est :\n")
  print(obj$shapiro)

  cat("\nLe test de l'homogénéité des variances est :\n")
  if (inherits(obj$levene, "try-error") || is.null(obj$levene)) {
    cat("Le test de Levene n'a pas pu être réalisé.\n")
  } else {
    print(obj$levene)
  }

  invisible(obj)
}


plot.anova2 <- function(obj, main = "QQ-plot des résidus", xlab = "Quantiles théoriques", ylab = "Résidus échantillons", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)

  df <- obj$data
  df$residuals <- obj$residuals

  p <- ggplot(df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(main) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}

data("iris")
resultat <- context_anova(iris,"Sepal.Length","Sepal.Width","Species")
print(resultat)
plot(resultat)

data("mtcars")
res <- context_anova(mtcars, "mpg", "cyl", "am")
print(res)
plot(res)

