
#' Construit une formule
#'
#' @param f1 character : membre de gauche 
#' @param f2 character : membre de droite 
#' @param operateur character : parmi +-*/
#'
#' @return character : formule agrégée
#' @export
#'
#' @examples
#' calculer_formule("5+3","10/2-7","*")
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
