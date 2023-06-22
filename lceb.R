library(dplyr)
library(purrr)
library(magrittr)
library(stringr)


# Test --------------------------------------------------------------------

nb_operandes <- 4
cible <- 152
liste_operandes <- c(5, 10, 3, 2)
rep_unit <- sum(10^(0:(nb_operandes - 1)))

resultats <- data.frame(
  res = liste_operandes,
  formule = as.character(liste_operandes),
  ingredients = 10^((nb_operandes - 1):0) + rep_unit
)
resultats %<>% head(1)


# Constantes --------------------------------------------------------------

df_vide <- data.frame(res = numeric(), formule = character(), ingredients = numeric())


# Fonctions ---------------------------------------------------------------

calculer_formule <- function(f1, f2, operateur) {
  part1 <- ifelse(
    str_detect(f1, "[*/+-]") & operateur %in% c("*", "/"),
    paste0("(", f1, ")"),
    f1
  )
  part2 <- ifelse(
    str_detect(f2, "[*/+-]") & operateur %in% c("*", "/", "-"),
    paste0("(", f2, ")"),
    f2
  )
  paste0(part1, operateur, part2)
}

sommer_ingredients <- function(ingredients1, ingredients) {
  ingredients1 + ingredients - rep_unit
}

peut_combiner <- function(ingredients1, ingredients) {
  # !grepl("4",as.character(ingredients1+ingredients))
  str_detect(as.character(ingredients1 + ingredients), "4", negate = TRUE)
}

peut_combiner(1211, 2111)
resultats %>% filter(peut_combiner(ingredients, 2111))

calculer_valeurs <- function(res1, formule1, ingredients1, res, formule, ingredients) {
  ingr <- sommer_ingredients(ingredients1, ingredients)
  # Addition
  r <- res + res1
  form <- calculer_formule(formule1, formule, "+")
  tab <- data.frame(res = r, formule = form, ingredients = ingr)
  # Multiplication
  r <- res * res1
  form <- calculer_formule(formule1, formule, "*")
  tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  # Soustraction
  r <- ifelse(res > res1, res - res1, res1 - res)
  form <- ifelse(res > res1,
    calculer_formule(formule, formule1, "-"),
    calculer_formule(formule1, formule, "-")
  )
  tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  # Division
  if (res %% res1 == 0) {
    r <- res %/% res1
    form <- calculer_formule(formule, formule1, "/")
    tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  }
  if (res1 %% res == 0) {
    r <- res1 %/% res
    form <- calculer_formule(formule1, formule, "/")
    tab %<>% add_row(res = r, formule = form, ingredients = ingr)
  }
  return(tab %>% filter(res != 0))
}
calculer_valeurs(5, "5", 2111, 7, "7", 1211)
calculer_valeurs(5, "5", 2111, 70, "7*10", 1221)

combiner <- function(res1, formule1, ingredients1, resultats) {
  res_trav <- resultats %>% filter(peut_combiner(ingredients1, ingredients))
  if (nrow(res_trav) == 0) {
    return(df_vide)
  }
  pmap_dfr(res_trav, calculer_valeurs, res1, formule1, ingredients1)
}

rechercher_tous <- function(res, formule, ingredients, resultats) {
  sol <- df_vide
  nouveaux_resultats <- combiner(
    res1 = res,
    formule1 = formule,
    ingredients1 = ingredients,
    resultats
  )
  sol %<>% bind_rows(nouveaux_resultats)
  print(sol)
  while (nrow(nouveaux_resultats) > 0) {
    nouveaux_resultats %<>% rename(res1 = res, formule1 = formule, ingredients1 = ingredients)
    nouveaux_resultats <- pmap_dfr(nouveaux_resultats, combiner, resultats)
    sol %<>% bind_rows(nouveaux_resultats)
  }
  return(sol)
}

ajouter_ingredient <- function(resultats, i, nb_operandes, liste_operandes) {
  res <- liste_operandes[i]
  formule <- as.character(res)
  ingredients <- 10^(nb_operandes - i) + rep_unit
  resultats %<>% add_row(res, formule, ingredients)
}

resoudre_lceb <- function(nb_operandes, liste_operandes, cible, avec_elagage = TRUE) {
  resultats <- df_vide
  rep_unit <<- sum(10^(0:(nb_operandes - 1)))
  for (i in 1:nb_operandes) {
    print(i)
    resultats %<>% ajouter_ingredient(i, nb_operandes, liste_operandes)
    nouv_res <- pmap(.l = resultats %>% tail(1),
          .f = rechercher_tous,
          resultats=resultats) %>% as.data.frame()

    # nouv_res <- rechercher_tous(
    #   res = liste_operandes[i],
    #   formule = as.character(liste_operandes[i]),
    #   ingredients = 10^(nb_operandes - i) + rep_unit,
    #   resultats = resultats
    # )

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


# Résolution --------------------------------------------------------------


mon_res <- resoudre_lceb(
  nb_operandes = 6,
  liste_operandes = c(1, 3, 100, 10, 75, 10),
  cible = 587
)

mon_res$sol
mon_res[[1]]
mon_res[[2]]
mon_res$res %>% filter(res == 7501)
mon_res$res %>% filter(ingredients == 211122)
mon_res$res %>% filter(ingredients == 211121)
mon_res$res %>% filter(res == 100)
