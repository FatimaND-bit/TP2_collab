#' Générateur minimal via Ollama
#'
#' @param prompt Chaîne de caractères. La question ou l'instruction à envoyer au modèle.
#' @param provider Chaîne de caractères. Le fournisseur de service.
#'   Actuellement, seul `"ollama"` est supporté (valeur par défaut).
#' @param model Chaîne de caractères. Le nom du modèle à utiliser (ex: "llama3.2:1b", "mistral", "gemma").
#'   Le modèle doit avoir été préalablement téléchargé via `ollama pull`.
#'   Défaut : `"llama3.2:1b"`.
#'
#' @returns Retourne la réponse générée par le modèle (le type exact dépend de `ellmer`, généralement une chaîne ou un objet chat),
#'   ou `NULL` si une erreur survient (package manquant, provider non supporté, ou échec de connexion).
#'
#' @importFrom ellmer chat_ollama
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
