require(tidyverse)
require(TAM)

data("data.fims.Aus.Jpn.scored")


Modele = 
  data.fims.Aus.Jpn.scored %>% 
  select(starts_with("M1PT")) %>% 
  TAM::tam.mml.2pl()

jpeg(filename = "TEST WrightMap.jpeg")
TAM::IRT.WrightMap(object = Modele)
dev.off()

Modele$item_irt %>% 
  mutate(Proportion = 1 - pnorm(q = beta,
                                mean = 0.2, sd = 0.94)) %>% 
  mutate(Pct = scales::percent(x = Proportion, 
                               accuracy = 0.1))


Erreurs = TAM::tam.se(tamobj = Modele)

# Les outils de distribution normale --------------------------------------


BORNE_1 = qnorm(p = 0.9, mean = 0, sd = 1)

pnorm(q = BORNE_1, mean = 0.2, sd = 0.94) %>% 
  scales::percent(accuracy = 0.1)


qnorm(p = 0.1)
pnorm()
dnorm()
rnorm(n = 1500, mean = 0, sd = 1) %>% hist()
