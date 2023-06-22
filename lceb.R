# Packages ----------------------------------------------------------------
source("chargement_packages.R")
# Fonctions ---------------------------------------------------------------
source("fonctions/fonctions_ingredients.R")
source("fonctions/fonctions_formules.R")


# Constantes --------------------------------------------------------------
df_vide <- data.frame(res = numeric(), formule = character(), ingredients = numeric())

# Fonctions de calcul -----------------------------------------------------

#' Réalise les opérations élémentaires
#'
#' @param res1 integer : un nombre entier positif
#' @param formule1 character : formule qui génère res1
#' @param ingredients1 integer : encodage des operandes utilisés pour générer res1 selon formule1
#' @param res integer : un nombre entier positif
#' @param formule character : formule qui génère res
#' @param ingredients integer : encodage des operandes utilisés pour générer res1 selon formule
#'
#' @return data.frame : les res, formule, ingredients, générés à partir de res1 et res
#' @export
#'
#' @examples
#' calculer_valeurs(5, "5", 2111, 7, "7", 1211)
#' calculer_valeurs(5, "5", 2111, 70, "7*10", 1221)
calculer_valeurs <- function(res1, formule1, ingredients1, res, formule, ingredients) {
  ingr <- sommer_ingredients(ingredients1, ingredients)
  # Addition
  r <- res + res1
  form <- calculer_formule(formule1, formule, "+")
  tab <- data.frame(res = r, formule = form, ingredients = ingr)
  # Multiplication
  if (res != 1 && res1 != 1) {
    r <- res * res1
    form <- calculer_formule(formule1, formule, "*")
    tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  }
  # Soustraction
  r <- ifelse(res > res1, res - res1, res1 - res)
  form <- ifelse(res > res1,
    calculer_formule(formule, formule1, "-"),
    calculer_formule(formule1, formule, "-")
  )
  tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  # Division
  if (res %% res1 == 0 && res1 != 1) {
    r <- res %/% res1
    form <- calculer_formule(formule, formule1, "/")
    tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  }
  if (res1 %% res == 0 && res != res1 && res != 1) {
    r <- res1 %/% res
    form <- calculer_formule(formule1, formule, "/")
    tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  }
  return(tab %>% filter(res != 0))
}


#' Combine un nombre avec les résultats antérieurs
#'
#' @param res1 integer : un nombre entier positif
#' @param formule1 character : formule qui génère res1
#' @param ingredients1 integer : encodage des operandes utilisés pour générer res1 selon formule1
#' @param resultats data.frame : les res, formule, ingredients, précédemment générés
#'
#' @return data.frame : les res, formule, ingredients, générés en une opération à partir de res1 et de resultats
#' @export
#'
#' @examples
combiner <- function(res1, formule1, ingredients1, resultats) {
  res_trav <- resultats %>% filter(peut_combiner(ingredients1, ingredients))
  if (nrow(res_trav) == 0) {
    return(df_vide)
  }
  pmap_dfr(res_trav, calculer_valeurs, res1, formule1, ingredients1)
}

#' Recherche en profondeur tous les nombre accessible à partir d'un nombre et des résultats antérieurs
#'
#' @param res integer : un nombre entier positif
#' @param formule character : formule qui génère res
#' @param ingredients integer : encodage des operandes utilisés pour générer res selon formule
#' @param resultats data.frame : les res, formule, ingredients, précédemment générés
#'
#' @return data.frame : les res, formule, ingredients, générés en une ou plusieurs opérations à partir de res et de resultats
#' @export
#'
#' @examples
rechercher_tous <- function(res, formule, ingredients, resultats) {
  print(paste0("rechercher_tous: ", res))
  sol <- df_vide
  nouveaux_resultats <- combiner(
    res1 = res,
    formule1 = formule,
    ingredients1 = ingredients,
    resultats
  )
  sol %<>% bind_rows(nouveaux_resultats)
  while (nrow(nouveaux_resultats) > 0) {
    print("deeper")
    nouveaux_resultats %<>% rename(res1 = res, formule1 = formule, ingredients1 = ingredients)
    nouveaux_resultats <- pmap_dfr(nouveaux_resultats, combiner, resultats)
    sol %<>% bind_rows(nouveaux_resultats)
  }
  return(sol)
}

#' Ajoute un opérande à la table des résultats
#'
#' @param resultats data.frame : les res, formule, ingredients, précédemment générés
#' @param i integer : indice de l'opérande à ajouter
#' @param liste_operandes vector of integers : les opérandes du problème
#' @param nb_operandes integer : optionnel, recalculé si manquant
#'
#' @return data.frame : les res, formule, ingredients, précédemment générés plus le nouvel opérande
#' @export
#'
#' @examples
ajouter_ingredient <- function(resultats, i, liste_operandes, nb_operandes = 0) {
  if (nb_operandes == 0) {
    nb_operandes <- length(liste_operandes)
  }
  res <- liste_operandes[i]
  formule <- as.character(res)
  ingredients <- 10^(nb_operandes - i) + creer_rep_unit(nb_operandes)
  resultats %<>% add_row(res, formule, ingredients)
}

# Résolution --------------------------------------------------------------

#' Résout un problème LCEB
#'
#' @param liste_operandes vector of integers : les opérandes du problème
#' @param cible integer : optionnel : nombre à atteindre
#' @param nb_operandes integer : optionnel, recalculé si manquant
#' @param avec_elagage logical : default TRUE : garde un seul des chemins équivalents pour générer une valeur avec les me^mes ingrédients.
#'
#' @return list : ..1: data.table des solutions pour atteindre la cible ..2: data.table des nombres accessibles
#' @export
#'
#' @examples
#' resoudre_lceb(
#'   nb_operandes = 4,
#'   liste_operandes = c(1, 5, 100, 8),
#'   cible = 813
#' )
#' resoudre_lceb(
#'   nb_operandes = 5,
#'   liste_operandes = c(1, 5, 100, 8, 75),
#'   cible = 577
#' )
resoudre_lceb <- function(liste_operandes, cible = -1, nb_operandes = 0, avec_elagage = TRUE) {
  if (nb_operandes == 0) {
    nb_operandes <- length(liste_operandes)
  }
  resultats <- df_vide
  for (i in 1:nb_operandes) {
    print(i)
    resultats %<>% ajouter_ingredient(i, liste_operandes)
    nouv_res <- pmap(
      .l = resultats %>% tail(1),
      .f = rechercher_tous,
      resultats = resultats
    ) %>%
      as.data.frame()

    sol <- nouv_res %>% filter(res == cible)

    if (avec_elagage) {
      print("elagage")
      nouv_res %<>% filter(!duplicated(res, ingredients))
    }
    resultats %<>% bind_rows(nouv_res)
    if (nrow(sol > 0)) {
      break
    }
  }
  list(sol = sol, res = resultats)
}



# Tests -------------------------------------------------------------------

mon_res <- resoudre_lceb(
  nb_operandes = 4,
  liste_operandes = c(1, 5, 100, 8),
  cible = 813
)

mon_res <- resoudre_lceb(
  nb_operandes = 5,
  liste_operandes = c(1, 5, 100, 8, 75),
  cible = 577
)

mon_res <- resoudre_lceb(
  nb_operandes = 6,
  liste_operandes = c(5, 6, 6, 9, 9, 10),
  cible = 568
)


mon_res$sol
mon_res$res
