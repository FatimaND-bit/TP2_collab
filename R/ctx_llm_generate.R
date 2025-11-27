#' Générateur minimal via Ollama
#' @export
ctx_llm_generate <- function(prompt, provider = "ollama", model = "llama3.2:1b") {
  
  # Vérifier si ellmer est installé
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    warning("Le package 'ellmer' n'est pas installé.")
    return(NULL)
  }
  
  # Seul Ollama est supporté
  if (provider != "ollama") {
    warning("Provider non supporté.")
    return(NULL)
  }
  
  # Création du client Ollama
  chat <- try(ellmer::chat_ollama(model = model), silent = TRUE)
  if (inherits(chat, "try-error")) {
    warning("Impossible de se connecter à Ollama. Vérifie qu'il est lancé.")
    return(NULL)
  }
  
  # Génération du texte
  out <- try(chat$chat(prompt), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning("Erreur lors de la génération via Ollama.")
    return(NULL)
  }
  
  return(out)
}
