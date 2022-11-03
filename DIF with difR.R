pacman::p_load(TAM, psych, tidyverse, difR)
data("data.fims.Aus.Jpn.scored")
input = data.fims.Aus.Jpn.scored %>% select(starts_with("M1")) 
contexte = data.fims.Aus.Jpn.scored %>% select(-starts_with("M1"))

difR::genDichoDif(Data = input, 
                  group = contexte$SEX,
                  focal.names = 1,
                  method = "genLogistic", 
                  type = "both")
