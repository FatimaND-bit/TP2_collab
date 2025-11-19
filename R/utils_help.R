#' Extraire un contexte depuis l'aide d'un dataset
#'
#' Tente de récupérer le texte d'aide associé à un jeu de données.
#' - `data_name` peut être un objet `data.frame` ou un nom de dataset (symbole ou chaîne).
#' - Si `package` est NULL, on essaie de retrouver automatiquement le package d'origine.
#' - On renvoie les sections les plus utiles (Description, Format) par défaut.
#' @param data_name data.frame ou nom du dataset
#' @param package Nom du package (chaîne), optionnel
#' @param sections Sections à extraire (regex), ex. c("^Description", "^Format")
#' @return Chaîne avec le contexte ; NULL si non trouvé.
#' @export
get_dataset_help <- function(
  data_name,
  package = NULL,
  sections = c("^Description", "^Format")
) {
  # 1. Trouver le nom du dataset
  topic <- if (is.data.frame(data_name)) {
    deparse(substitute(data_name))
  } else if (is.character(data_name)) {
    data_name
  } else {
    as.character(substitute(data_name))
  }

  # 2. Trouver automatiquement le package si non fourni
  if (is.null(package)) {
    all_data <- utils::data(package = .packages(all.available = TRUE))$results
    pkgs <- all_data[all_data[, "Item"] == topic, "Package"]
    if (length(pkgs)) {
      package <- pkgs[1]
    } else {
      warning("Impossible d'identifier le package contenant '", topic, "'.")
      return(NULL)
    }
  }

  # 3. Toujours convertir en chaîne de caractères
  package <- as.character(package)[1]

  # 4. Charger temporairement le package si nécessaire
  if (!paste0("package:", package) %in% search()) {
    suppressPackageStartupMessages(
      try(library(package, character.only = TRUE), silent = TRUE)
    )
  }

  # ⚠️ 5. Appel explicite à help() en évaluant le nom du package
  h <- try(do.call(utils::help, list(topic, package = package)), silent = TRUE)
  if (inherits(h, "try-error") || length(h) == 0) {
    warning(
      "Aucun fichier d'aide trouvé pour ",
      topic,
      " (package '",
      package,
      "')."
    )
    return(NULL)
  }

  # 6. Convertir le fichier Rd en texte
  rd_path <- utils:::.getHelpFile(h)
  rd_text <- capture.output(tools::Rd2txt(rd_path))

  # 7. Extraire les sections utiles
  keep <- logical(length(rd_text))
  for (sec in sections) {
    start <- grep(sec, rd_text, ignore.case = TRUE)
    if (length(start)) {
      end <- c(grep("^[A-Z].*:$", rd_text), length(rd_text) + 1)
      end <- min(end[end > start[1]]) - 1
      keep[start[1]:end] <- TRUE
    }
  }

  paste(rd_text[if (any(keep)) keep else TRUE], collapse = "\n")
}
