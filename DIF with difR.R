pacman::p_load(TAM, psych, tidyverse, difR)
data("data.fims.Aus.Jpn.scored")
input = data.fims.Aus.Jpn.scored %>% select(starts_with("M1")) 
contexte = data.fims.Aus.Jpn.scored %>% select(-starts_with("M1"))

difR::genDichoDif(Data = input, 
                  group = contexte$country,
                  focal.names = 1,
                  engine = "lme4",
                  method = "genLogistic", 
                  type = "both")
