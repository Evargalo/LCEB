source("lceb.R")

list_op <- c(3,7,75,100,100)

essai<-resoudre_lceb(
  liste_operandes = list_op,
  avec_elagage = FALSE
)
essai$res %>% distinct(res) %>% 
  filter(str_detect(formule,"\\/\\(")) -> pb
pb %>% arrange(desc(res))
