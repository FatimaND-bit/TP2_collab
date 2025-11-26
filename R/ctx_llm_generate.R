# #' Title
# #'
# #' @param prompt
 # #' @param provider
# #' @param model

# #' @returns
# #' @export
##'
# #' @examples
ctx_llm_generate <- function(prompt, provider = "ollama", model = "mistral") {
  if (provider == "ollama") {
    chat <- ellmer::chat_ollama(model = model)
  } else {
    message("Le modèle n'est pas disponible")
  }
  return(chat$chat(prompt))
}
