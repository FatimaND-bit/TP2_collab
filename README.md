# TP #2 — Package R : Analyse statistique contextuelle avec LLM

Pour le cours STT-4230 — Université Laval

**Nom du package :** llmstats

# 👥 Équipe

  - Nathan Tremblay
  - Marck-Land Ahouansou
  - Saa Moussa Tenguiano
  - Fatima Ndiaye

# 🎯 Objectif du package

Le package llmstats ajoute une couche « contextuelle » aux analyses statistiques classiques dans R.
Il permet de :

  - effectuer des analyses (ANOVA, corrélations, etc.) ;
  - enrichir les résultats avec une interprétation générée automatiquement ;
  - utiliser un contexte fourni par l’utilisateur ou extrait de l’aide du dataset ;
  - intégrer un modèle de langage (LLM) local via Ollama pour produire des explications.

Ce TP vise à démontrer :

  - la maîtrise des méthodes S3 (print, summary, plot),
  - la conception d’un package R minimal,
  - l’intégration d’un LLM,
  - la capacité à contextualiser une analyse statistique.

# Fonctionnalités principales du package
## 1. context_anova()

Effectue une ANOVA classique et retourne un objet enrichi contenant :

  - le modèle,
  - la table ANOVA,
  - le contexte textuel,
  - une interprétation automatique (si un LLM est disponible).

Méthodes S3 associées :

  - print.context_anova : résumé + courte interprétation ;
  - summary.context_anova : interprétation détaillée ;
  - plot.context_anova : boxplot automatique (sans LLM).

## 2. cor_context()

Calcule une matrice de corrélation et identifie les paires les plus corrélées.

Méthodes S3 associées :

  - print.context_cor : matrice + top corrélations ;
  - summary.context_cor : distribution des corrélations + interprétation ;
  - plot.context_cor : heatmap simple (sans LLM).

## 3. ctx_llm_generate()

Fonction interne qui envoie un prompt à un modèle LLM local via Ollama
(ex. mistral, llama3, etc.) et retourne une interprétation textuelle.

## 4. get_dataset_help()

Extrait automatiquement les sections importantes (Description, Format)
de l’aide d’un dataset R pour servir de contexte aux analyses.

# Prérequis logiciels

R ≥ 4.2

Packages R :
stats, utils, tools, ggplot2, ellmer, jsonlite, httr

Ollama (recommandé)
pour exécuter un modèle LLM localement.

Une fois Ollama installé, tester avec :
`ollama run mistral "Bonjour"`

# Exemple de flux de travail avec le package

Charger un dataset (ex. penguins).

Extraire automatiquement le contexte :
`ctx <- get_dataset_help("penguins", "palmerpenguins")`

Effectuer l’analyse :
`res <- context_anova(body_mass_g ~ species, data = penguins, context = ctx)`

Consulter les résultats :

  - `print(res)`
  - `summary(res)`
  - `plot(res)`

# Ce que démontre ce TP

  - Création d’un package R fonctionnel
  - Utilisation et surcharge de méthodes S3
  - Génération d’interprétations dynamiques via un LLM
  - Intégration du contexte dans l’analyse statistique
  - Documentation claire et reproductibilité

# Licence

Ce projet est distribué sous la licence MIT, libre et permissive.

# Remarque finale

Le package llmstats constitue une démonstration complète d’analyse statistique augmentée par intelligence artificielle dans R.
Il peut être étendu facilement à d’autres méthodes : modèles linéaires, tests, visualisations, etc.
