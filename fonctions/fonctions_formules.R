
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
