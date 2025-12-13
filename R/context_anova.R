#' ANOVA contextuelle
#'
#' Cette fonction applique un modèle ANOVA (via \code{aov()}) à une formule
#' du type \code{y ~ groupe} et retourne un objet enrichi contenant :
#' - le modèle ajusté,
#' - la table ANOVA,
#' - le contexte textuel (optionnel),
#' - une interprétation générée par un LLM (si disponible).
#'
#' @param formula Formule du modèle ANOVA (ex: \code{body_mass_g ~ species}).
#' @param data Jeu de données contenant les variables.
#' @param context Texte libre décrivant le contexte de l'analyse.
#' @param ... Arguments supplémentaires transmis à \code{aov()}.
#'
#' @return Un objet de classe \code{context_anova}.
#' @export
context_anova <- function(formula, data, context = NULL, ...) {

  if (missing(formula) || missing(data)) {
    stop("Veuillez fournir une formule et un jeu de données.", call. = FALSE)
  }

  # Ajustement du modèle ANOVA
  model <- stats::aov(formula, data = data, ...)
  tab <- summary(model)[[1]]

  # Objet structuré
  out <- list(
    model   = model,
    table   = tab,
    formula = formula,
    data    = data,
    context = context
  )

  class(out) <- "context_anova"
  return(out)
}

# =====================================================================
# Méthode PRINT
# =====================================================================

#' @export
print.context_anova <- function(x, digits = 3, ...) {

  cat("=== Résultat ANOVA contextuelle ===\n\n")
  cat("Formule :", deparse(x$formula), "\n\n")

  tab <- x$table
  tab[, 1:4] <- round(tab[, 1:4], digits)

  print(tab)
  cat("\n")

  # Interprétation simple via LLM (si disponible)
  if (exists("ctx_llm_generate", mode = "function")) {

    prompt <- paste(
      "Voici une table ANOVA :",
      paste(capture.output(print(tab)), collapse = "\n"),
      if (!is.null(x$context)) paste("\nContexte :", x$context) else "",
      "\nRédige 2-3 phrases expliquant les résultats de manière claire et pédagogique.",
      sep = "\n"
    )

    cat("Interprétation (LLM) :\n")
    msg <- try(ctx_llm_generate(prompt), silent = TRUE)
    if (inherits(msg, "try-error") || is.null(msg)) {
      cat("(Impossible de générer une interprétation via le LLM.)\n")
    } else {
      cat(msg, "\n")
    }
  }

  invisible(x)
}

# =====================================================================
# Méthode SUMMARY
# =====================================================================

#' @export
summary.context_anova <- function(object, digits = 3, ...) {

  tab <- object$table
  tab[, 1:4] <- round(tab[, 1:4], digits)

  cat("=== Summary(context_anova) ===\n\n")
  cat("Formule :", deparse(object$formula), "\n\n")

  print(tab)
  cat("\n")

  # Interprétation détaillée via LLM
  if (exists("ctx_llm_generate", mode = "function")) {

    prompt <- paste(
      "Analyse détaillée d'une ANOVA.",
      "\nTable ANOVA :",
      paste(capture.output(print(tab)), collapse = "\n"),
      if (!is.null(object$context)) paste("\nContexte :", object$context) else "",
      "\nRédige une interprétation complète (4-7 phrases), en expliquant :",
      "- l’effet du facteur sur la variable réponse ;",
      "- la signification de F et de la p-value ;",
      "- si l’effet semble important dans le contexte.",
      sep = "\n"
    )

    cat("Interprétation détaillée (LLM) :\n")
    msg <- try(ctx_llm_generate(prompt), silent = TRUE)
    if (inherits(msg, "try-error") || is.null(msg)) {
      cat("(Impossible de générer une interprétation via le LLM.)\n")
    } else {
      cat(msg, "\n")
    }
  }

  invisible(object)
}

# =====================================================================
# Méthode PLOT
# =====================================================================


#' @export
plot.context_anova <- function(x, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package ggplot2 est requis pour plot.context_anova().")
  }

  data <- x$data
  y_var <- all.vars(x$formula)[1]
  group_var <- all.vars(x$formula)[2]

  # Graphique simple : boxplot par groupe
  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(.data[[group_var]]),
                                          y = .data[[y_var]])) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Distribution de", y_var, "selon", group_var),
      x = group_var,
      y = y_var
    )

  print(p)
  invisible(x)
}
