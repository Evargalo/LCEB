
#' crée un nombre formé de '1'
#'
#' @param nb_operandes integer : nombre voulu de 1
#'
#' @return integer
#' @export
#'
#' @examples
#' creer_rep_unit(5)
creer_rep_unit<-function(nb_operandes){
  sum(10^(0:(nb_operandes - 1)))
}


#' Calcule et encode les ingrédients d'une formule
#'
#' @param ingredients1 integer : ingrédients du membre de gauche
#' @param ingredients integer : ingrédients du membre de droite
#' @param rep_unit integer : optional
#'
#' @return integer : ingrédients
#' @export
#'
#' @examples
#' sommer_ingredients(12221,21111)
sommer_ingredients <- function(ingredients1, ingredients, rep_unit=0) {
  if(rep_unit==0) {
    rep_unit <- creer_rep_unit(nchar(ingredients))
  }
  ingredients1 + ingredients - rep_unit
}


#' Teste la possibilité de combiner deux membres
#'
#' @param ingredients1 integer : ingrédients du membre de gauche
#' @param ingredients integer : ingrédients du membre de droite
#'
#' @return logical : peut-on combiner les deux membres ?
#' @export
#'
#' @examples
#' peut_combiner(1211, 2111)
peut_combiner <- function(ingredients1, ingredients) {
  str_detect(as.character(ingredients1 + ingredients), "4", negate = TRUE)
}

peut_combiner(1211, 2111)


#' vérifie si un operande est utilisé
#'
#' @param ingredients : integer : encodage des opérandes utilisés
#' @param i : integer : rang de l'opérande cherché
#'
#' @return logical
#' @export
#'
#' @examples
utilise_ingr <- function(ingredients,i) {
  str_sub(as.character(ingredients),i,i)=="2"
}
utilise_ingr(122111221,4)
utilise_ingr(122111221,3)
