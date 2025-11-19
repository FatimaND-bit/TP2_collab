source("R/ctx_llm_generate.R")
source("R/utils_help.R")

#' Title
#'
#' @param object
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
context_anova <- function(object, ...){
  out <- anova(object, ...)
  class(out) <- "context_anova"
  return(out)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
summary.context_anova <- function(x, ...){
  # Création de la sortie
  tab <- data.frame("Degré de liberté" = x$Df,
                    "Somme de carrés" = x$`Sum Sq`,
                    "Carrés moyens" = x$`Mean Sq`,
                    "Valeur F" = x$`F value`,
                    "P-value" = x$`Pr(>F)`,
                    row.names = c("dist", "residuals")) # À changer pour que les bons noms apparaissent
  tab[, 1:4] <- round(tab[,1:4], 3)
  tab[, 5] <- signif(tab[, 5], digits = 5)
  tab[is.na(tab)] <- ""

  # Impression de la sortie
  print(tab)

  # Mise en texte du jeu de données
  context <- get_dataset_help(data = cars) # À changer pour que les bons noms apparaissent

  # Création de l'analyse générée par un LLM (À BONNIFIER)
  cat("Contexte généré par LLM :\n")
  ctx_llm_generate(paste("Tu est un statisticien de renommé mondial. On te donne une table
  ANOVA pour que tu l'analyse. En plus de la table ANOVA, tu auras également le jeu de données
  initial. Tu dois interpréter, en français, les résultats de la table ANOVA en fonction du context du
  jeu de données. Voici le jeu de données :", context, "Et voici la table ANOVA: ", tab))

  # Moyen d'éviter que x soit afficher 2 fois
  invisible(x)
}
