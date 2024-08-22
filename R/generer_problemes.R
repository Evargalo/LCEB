source("lceb.R")

list_op <- c(3,7,75,100,100)
list_op <- c(1,3,7,10,75,100)

essai<-resoudre_lceb(
  liste_operandes = list_op,
  avec_elagage = FALSE
)
essai$res %>% distinct(res) -> resu

essai$res %>% 
  filter(str_detect(formule,"\\/\\(")) -> pb
pb %>% filter(res %in% resu$res) %>% arrange(desc(res))

essai$sol
