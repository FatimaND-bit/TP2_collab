#' Crée un objet context_cor à partir d'une matrice de corrélation
#'
#' Cette fonction calcule une matrice de corrélation à partir d'un jeu de données,
#' identifie les paires de variables les plus corrélées et stocke ces informations
#' dans un objet de classe \code{context_cor}. Cet objet pourra ensuite être
#' résumé et interprété grâce aux méthodes S3 \code{print()} et \code{summary()},
#' avec éventuellement l'aide d'un modèle de langage (LLM).
#'
#' @param data data.frame contenant les variables d'intérêt.
#' @param vars vecteur de noms de variables à sélectionner. Si \code{NULL},
#'   toutes les variables numériques de \code{data} sont utilisées.
#' @param method méthode de corrélation : \code{"pearson"}, \code{"spearman"} ou
#'   \code{"kendall"}.
#' @param use argument \code{use} passé à \code{stats::cor()} pour gérer les
#'   valeurs manquantes (par ex. \code{"pairwise.complete.obs"}).
#' @param context texte libre décrivant le contexte de l'étude
#'   (question de recherche, description des données, etc.).
#'
#' @return Un objet de classe \code{context_cor} contenant au minimum :
#' \itemize{
#'   \item \code{cor_mat} : matrice de corrélation.
#'   \item \code{top_pairs} : data.frame des paires de variables
#'         triées par corrélation absolue décroissante.
#'   \item \code{data} : données utilisées pour le calcul.
#'   \item \code{method}, \code{use}, \code{context} : métadonnées.
#' }
#' @export
cor_context <- function(data, vars = NULL,
                        method = c("pearson", "spearman", "kendall"),
                        use = "pairwise.complete.obs",
                        context = NULL) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("`data` doit être un data.frame.", call. = FALSE)
  }

  # Sélection des variables
  if (is.null(vars)) {
    num_cols <- sapply(data, is.numeric)
    if (!any(num_cols)) {
      stop("Aucune variable numérique trouvée dans `data`.", call. = FALSE)
    }
    data_use <- data[, num_cols, drop = FALSE]
  } else {
    vars <- intersect(vars, names(data))
    if (length(vars) < 2) {
      stop("Il faut au moins deux variables dans `vars` pour calculer des corrélations.", call. = FALSE)
    }
    data_use <- data[, vars, drop = FALSE]
  }

  # Matrice de corrélation
  cor_mat <- stats::cor(data_use, use = use, method = method)

  # Préparation des paires pour trouver les plus fortes corrélations
  p <- ncol(cor_mat)
  if (p < 2) {
    stop("Il faut au moins deux variables numériques pour calculer une matrice de corrélation.", call. = FALSE)
  }

  pairs <- which(upper.tri(cor_mat), arr.ind = TRUE)
  top_df <- data.frame(
    var1  = colnames(cor_mat)[pairs[, "row"]],
    var2  = colnames(cor_mat)[pairs[, "col"]],
    r     = as.numeric(cor_mat[pairs]),
    abs_r = abs(as.numeric(cor_mat[pairs])),
    stringsAsFactors = FALSE
  )
  top_df <- top_df[order(-top_df$abs_r), , drop = FALSE]

  if (is.null(context)) context <- ""

  out <- list(
    cor_mat   = cor_mat,
    top_pairs = top_df,
    data      = data_use,
    method    = method,
    use       = use,
    context   = context
  )
  class(out) <- "context_cor"
  out
}

#' Affichage d'un objet context_cor
#'
#' La méthode \code{print()} pour la classe \code{context_cor} affiche
#' un résumé de la matrice de corrélation, ainsi que les corrélations
#' les plus fortes. Si une fonction \code{ctx_llm_generate()} est disponible
#' dans le package, une courte interprétation textuelle est générée.
#'
#' @param x objet de classe \code{context_cor}.
#' @param n_top nombre de paires les plus corrélées à afficher.
#' @param digits nombre de décimales pour l'affichage de la matrice.
#' @param ... arguments supplémentaires (ignorés).
#'
#' @export
print.context_cor <- function(x, n_top = 3, digits = 2, ...) {
  if (!inherits(x, "context_cor")) {
    stop("`x` doit être un objet de classe 'context_cor'.", call. = FALSE)
  }

  cat("=== Matrice de corrélation contextuelle ===\n\n")
  cat(sprintf("Méthode : %s | Variables : %d | use = %s\n\n",
              x$method, ncol(x$cor_mat), x$use))

  # Matrice arrondie
  cor_round <- round(x$cor_mat, digits = digits)
  print(cor_round)
  cat("\n")

  # Top corrélations
  if (!is.null(x$top_pairs) && nrow(x$top_pairs) > 0) {
    n_top <- min(n_top, nrow(x$top_pairs))
    top_n <- x$top_pairs[seq_len(n_top), , drop = FALSE]

    cat(sprintf("Top %d corrélations (en valeur absolue) :\n", n_top))
    apply(top_n, 1, function(row) {
      cat(sprintf("- %s ~ %s : r = %.3f\n",
                  row[["var1"]], row[["var2"]],
                  as.numeric(row[["r"]])))
    })
    cat("\n")

    # Interprétation courte via LLM (si disponible)
    if (exists("ctx_llm_generate", mode = "function")) {
      prompt <- paste(
        "On a calculé une matrice de corrélation entre variables numériques.",
        "Voici les corrélations les plus fortes :",
        paste(
          sprintf("%s ~ %s : r = %.3f",
                  top_n$var1, top_n$var2, top_n$r),
          collapse = "; "
        ),
        if (!is.null(x$context) && nzchar(x$context)) {
          paste("\nContexte de l'étude :", x$context)
        } else {
          ""
        },
        "\nRédige 2-3 phrases en français pour résumer brièvement les relations les plus importantes.",
        sep = "\n"
      )

      cat("Interprétation (LLM) :\n")
      msg <- try(ctx_llm_generate(prompt), silent = TRUE)
      if (inherits(msg, "try-error") || is.null(msg)) {
        cat("(Impossible de générer une interprétation via le LLM.)\n")
      } else {
        cat(msg, "\n")
      }
    } else {
      cat("(Aucune fonction 'ctx_llm_generate()' trouvée : interprétation LLM non générée.)\n")
    }
  } else {
    cat("Pas assez de paires de variables pour calculer des corrélations.\n")
  }

  invisible(x)
}

#' Résumé détaillé d'un objet context_cor
#'
#' La méthode \code{summary()} pour la classe \code{context_cor} fournit
#' un résumé plus détaillé de la distribution des corrélations (min, médiane,
#' max, etc.), le nombre de corrélations fortes ou modérées, et, si possible,
#' une interprétation textuelle générée par un LLM en fonction du contexte.
#'
#' @param object objet de classe \code{context_cor}.
#' @param strong seuil (en valeur absolue) à partir duquel une corrélation
#'   est considérée comme forte.
#' @param moderate seuil (en valeur absolue) à partir duquel une corrélation
#'   est considérée comme modérée.
#' @param ... arguments supplémentaires (ignorés).
#'
#' @export
summary.context_cor <- function(object,
                                strong = 0.7,
                                moderate = 0.4,
                                ...) {
  if (!inherits(object, "context_cor")) {
    stop("`object` doit être un objet de classe 'context_cor'.", call. = FALSE)
  }

  cor_mat <- object$cor_mat

  upper_idx <- which(upper.tri(cor_mat), arr.ind = TRUE)
  if (length(upper_idx) == 0) {
    cat("Pas assez de variables pour résumer les corrélations.\n")
    return(invisible(object))
  }

  r_vals <- cor_mat[upper.tri(cor_mat)]
  abs_r  <- abs(r_vals)

  stats <- c(
    min  = min(r_vals, na.rm = TRUE),
    q1   = as.numeric(stats::quantile(r_vals, 0.25, na.rm = TRUE)),
    med  = stats::median(r_vals, na.rm = TRUE),
    mean = mean(r_vals, na.rm = TRUE),
    q3   = as.numeric(stats::quantile(r_vals, 0.75, na.rm = TRUE)),
    max  = max(r_vals, na.rm = TRUE)
  )

  n_strong   <- sum(abs_r >= strong, na.rm = TRUE)
  n_moderate <- sum(abs_r >= moderate & abs_r < strong, na.rm = TRUE)

  cat("=== Summary(context_cor) ===\n\n")
  cat(sprintf("Nombre de variables : %d | Paires possibles : %d\n\n",
              ncol(cor_mat), length(r_vals)))

  cat("Distribution des corrélations (hors diagonale) :\n")
  print(round(stats, 3))
  cat("\n")

  cat(sprintf("Corrélations fortes (|r| >= %.2f)           : %d paires\n", strong, n_strong))
  cat(sprintf("Corrélations modérées (%.2f <= |r| < %.2f) : %d paires\n\n",
              moderate, strong, n_moderate))

  # Lister quelques corrélations fortes
  df_pairs <- object$top_pairs
  strong_pairs <- df_pairs[df_pairs$abs_r >= strong, , drop = FALSE]
  if (nrow(strong_pairs) > 0) {
    cat("Paires fortement corrélées (extrait) :\n")
    head_strong <- utils::head(strong_pairs, 10)
    apply(head_strong, 1, function(row) {
      cat(sprintf("- %s ~ %s : r = %.3f\n",
                  row[["var1"]], row[["var2"]],
                  as.numeric(row[["r"]])))
    })
    cat("\n")
  }

  # Interprétation via LLM (si disponible)
  if (exists("ctx_llm_generate", mode = "function")) {
    prompt <- paste(
      "On a calculé une matrice de corrélation entre variables numériques.",
      "Résumé statistique des corrélations (min, médiane, max, etc.) :",
      paste(capture.output(print(round(stats, 3))), collapse = "\n"),
      sprintf("Nombre de corrélations fortes (|r| >= %.2f) : %d", strong, n_strong),
      sprintf("Nombre de corrélations modérées (%.2f <= |r| < %.2f) : %d",
              moderate, strong, n_moderate),
      if (nrow(strong_pairs) > 0) {
        paste(
          "\nPaires fortement corrélées :",
          paste(
            sprintf("%s ~ %s : r = %.3f",
                    strong_pairs$var1, strong_pairs$var2, strong_pairs$r),
            collapse = "; "
          )
        )
      } else {
        "\nIl n'y a pas de corrélations très fortes."
      },
      if (!is.null(object$context) && nzchar(object$context)) {
        paste("\nContexte de l'étude :", object$context)
      } else {
        ""
      },
      "\nRédige une interprétation détaillée en français pour un public non spécialiste :",
      "• décrire globalement le niveau de corrélation ;",
      "• commenter les paires les plus corrélées ;",
      "• signaler les éventuels risques (redondance, multicolinéarité).",
      sep = "\n"
    )

    cat("Interprétation détaillée (LLM) :\n")
    msg <- try(ctx_llm_generate(prompt), silent = TRUE)
    if (inherits(msg, "try-error") || is.null(msg)) {
      cat("(Impossible de générer une interprétation via le LLM.)\n")
    } else {
      cat(msg, "\n")
    }
  } else {
    cat("(Aucune fonction 'ctx_llm_generate()' trouvée : interprétation LLM non générée.)\n")
  }

  invisible(object)
}
