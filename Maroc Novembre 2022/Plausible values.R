source(file = "Maroc Novembre 2022/Fit et dependances TIMSS.R",
       echo = TRUE, encoding = "UTF-8")


# Plausible values --------------------------------------------------------
Plausible_values =
  TAM::tam.pv(tamobj = Modele_2D,
              nplausible = 5,
              verbose = TRUE)

Scores = 
  data.timss11.G4.AUT$data %>% 
  select(contains("id")) %>% 
  mutate_all(.funs = as.character) %>% 
  cbind.data.frame(Poids,
                   Plausible_values$pv)

# Survey design -----------------------------------------------------------
require(srvyr)
Design = 
  Scores %>% 
  pivot_longer(cols = starts_with("PV"), 
               names_to = "PV", values_to = "Score") %>% 
  as_survey_design(ids = c(IDCLASS, IDSCHOOL),
                   strata = IDCNTRY, 
                   nest = TRUE, 
                   weights = Poids) %>% 
  as_survey_rep(type = "BRR")


summary(Design)

Moyenne_PV = 
  Design %>% 
  group_by(PV) %>% 
  summarise(
    M = survey_mean(x = Score, vartype = "var"),
    SD = survey_sd(x = Score))



# Erreur de la moyenne ----------------------------------------------------
Get_erreur <- function(Moyenne_PV) {
  M = mean(Moyenne_PV$M)
  
  Var_sondage = mean(Moyenne_PV$M_var)
  
  Var_mesure = 1/4*sum((M - Moyenne_PV$M)^2)
  
  Var_total = Var_sondage + (1/5)*Var_mesure
  
  cbind.data.frame(M = M, erreur = sqrt(Var_total))
}

Get_erreur(Moyenne_PV = Moyenne_PV)


# Erreur par école --------------------------------------------------------

Moyenne_PV_SCHOOL = 
  Design %>% 
  group_by(IDSCHOOL, PV) %>% 
  summarise(
    M = survey_mean(x = Score, vartype = "var"),
    SD = survey_sd(x = Score))

Erreur_SCHOOL = 
  Moyenne_PV_SCHOOL %>% 
  group_by(IDSCHOOL) %>% 
  group_nest() %>% 
  mutate(Erreurs = map(.x = data, .f = Get_erreur)) %>% 
  select(-data) %>% unnest() 

Erreur_SCHOOL %>% 
  ggplot(mapping = aes(x = M, y = erreur)) +
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = IDSCHOOL))

