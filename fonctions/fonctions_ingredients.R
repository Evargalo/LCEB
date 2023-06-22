
creer_rep_unit<-function(nb_operandes){
  sum(10^(0:(nb_operandes - 1)))
}


sommer_ingredients <- function(ingredients1, ingredients, rep_unit=0) {
  if(rep_unit==0) {
    rep_unit <- creer_rep_unit(nchar(ingredients))
  }
  ingredients1 + ingredients - rep_unit
}
sommer_ingredients(12221,21111)

peut_combiner <- function(ingredients1, ingredients) {
  str_detect(as.character(ingredients1 + ingredients), "4", negate = TRUE)
}

peut_combiner(1211, 2111)