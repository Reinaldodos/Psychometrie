pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input = data.fims.Aus.Jpn.scored%>% select(starts_with("M")) %>% 
  select(-M1PTI12, -M1PTI14, -M1PTI21) 
  
contexte = data.fims.Aus.Jpn.scored %>% select(-starts_with("M")) %>% 
  mutate_all(.funs = as.factor)

Modele =
  tam.mml.2pl(
    resp = input,
    dataY = contexte, formulaY =  ~ SEX*country
  )

Modele$latreg_stand
Erreurs = Modele %>% tam.se()

cbind.data.frame(param = Modele$latreg_stand$beta_stand$parm,
                 Erreurs$beta) %>%
  mutate(zscore = est.Dim1 / se.Dim1) %>%
  mutate(p.value = pnorm(q = -abs(zscore))) %>%
  filter(p.value < .05)

Scores = Modele %>% tam.wle() %>% data.table() %>% 
  inner_join(contexte %>%
  rowid_to_column(var = "pid"))

Scores %>%
  mutate(VAR = error ^ 2) %>%
  mutate_at(.vars = vars(SEX, country), .funs = as.factor) %>%
  group_by(SEX,country) %>%
  summarise(M = mean(theta),
            SE = sqrt(mean(VAR) / n())) %>% ungroup %>%
  mutate(M=50*M+250,
         SE=50*SE) %>% 
  mutate(lower = M + qnorm(.05 / 2) * SE,
         upper = M - qnorm(.05 / 2) * SE) %>%
  mutate_if(.predicate = is.numeric,
            .funs = round,
            digit = 1) %>%
  data.table()

# sans latreg -------------------------------------------------------------
MODI =
  tam.mml.2pl(resp = input,
              group = contexte$country
              ) 

SKOR = MODI %>% 
  tam.wle() %>% data.table() %>%
  inner_join(contexte %>%
               rowid_to_column(var = "pid")) %>% 
  lm(formula = theta~SEX*country) %>% 
  broom::augment()

cbind.data.frame(Scores %>% data.table() %>% select(thetha_reg = theta),
                 SKOR %>% select(theta, .resid),
                 Modele_EAP=Modele$person$EAP,
                 MODI_EAP=MODI$person$EAP) %>%
  cor() %>% 
  ggcorrplot::ggcorrplot(hc.order = T)



plot(SKOR$theta, SKOR$.resid)


IRT.compareModels(Modele, MODI)