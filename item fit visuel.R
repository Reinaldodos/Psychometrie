FitPlot <- function(model, data, ...) {
  Scores = model %>% TAM::tam.wle() %>% data.table::data.table()
  
  Param = model$item_irt %>% as.list()
  names(Param$alpha) = Param$item
  names(Param$beta) = Param$item
  
  pacman::p_load(xxIRT)
  xxIRT::model_3pl_fitplot(
    u = data %>% as.matrix(),
    t = Scores$theta,
    a = Param$alpha,
    b = Param$beta,
    c = 0,
    D = 1.7, ...
  ) %>%
    return()
}

pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input = 
  data.fims.Aus.Jpn.scored %>% select(starts_with("M"))
  select(-M1PTI12, -M1PTI14, -M1PTI21)

input %>% psych::describe()

input %>% rowSums() %>% hist()
input %>% rowSums() %>% GLDEX::fun.moments.r(normalise = "Y")

Modeles = list("Rasch" = tam.mml,
               "2PL" = tam.mml.2pl,
               "3PL"  = tam.mml.3pl)

output = invoke_map(.f = Modeles, resp = input, verbose = F)
outputFIT = map(.x = output, .f = IRT.modelfit)
outputFIT %>% map(summary)

IRT.compareModels(output$Rasch, output$`2PL`, output$`3PL`)

outputFIT %>% map(.f = ~ .$stat.itempair %>% filter(abs(aQ3) > .2))

output$Rasch %>% plot()

output$Rasch %>% FitPlot(data = input)
output$Rasch %>% FitPlot(data = input)
output$`2PL` %>% FitPlot(data = input)

outputFIT$`2PL`$chisquare.itemfit
