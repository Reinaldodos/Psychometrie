pacman::p_load(TAM, psych, tidyverse, data.table)

data("data.fims.Aus.Jpn.scored", package = "TAM")
DATA = data.fims.Aus.Jpn.scored %>% select(starts_with("M1"))

Modele = tam.fa(resp = DATA,
                irtmodel = "efa",
                nfactors = 3, 
                control = list(increment.factor = 1.2))

Modele$meas

Modele$efa.oblimin$loadings %>% fa.diagram(cut = .1)
Modele$B.SL %>% omega.diagram(sl = T, cut = .3, gcut = .3)

Items_high_order =
  cbind.data.frame(item = rownames(Modele$B.SL),
                   Modele$B.SL) %>%
  gather(-item, key = "Factor", value = "loadings") %>% 
  filter(abs(loadings)>.3) %>% 
  spread(key = Factor, value = loadings) %>% 
  drop_na("g") %>% select(-g)

Dims = 
  Items_high_order %>%
  gather(
    -item,
    key = Factor,
    value = loadings,
    na.rm =  T
  ) %>%
  left_join(x = Items_high_order %>% select(item)) %>% 
  pull(Factor)


ModeleBifac =
  DATA %>%
  select(Items_high_order$item %>% as.character()) %>%
  tam.fa(
    irtmodel = "bifactor2",
    dims = Dims,
    control = list(increment.factor = 1.2)
  )

ModeleBifac$meas
ModeleBifac %>% summary()

FIT_bifac = ModeleBifac %>% IRT.modelfit()
FIT_bifac %>% summary()

FIT_bifac$chi2.stat

Modele_Uni =
  DATA %>%
  select(Items_high_order$item %>% as.character()) %>%
  tam.mml.2pl(irtmodel = "2PL",
              control = list(increment.factor = 1.2))

FIT = Modele_Uni %>% IRT.modelfit()
FIT %>% summary()
FIT$chi2.stat %>%
  filter(p.holm < 1) %>%
  inner_join(x = FIT$stat.itempair,
             by = c("index1", "index2"))

IRT.compareModels(ModeleBifac, Modele_Uni, Modele)
<<<<<<< HEAD:Analyse dimensionnelle/Analyse bifactorielle.R

list(Bifac = ModeleBifac,
     Uni = Modele_Uni) %>%
  map(.f = ~ .$item) %>%
  bind_rows(.id = "Modele") %>%
  ggplot(mapping = aes(x = AXsi_.Cat1, y = B.Cat1.Dim1)) +
  geom_point(mapping = aes(shape = Modele, colour = item)) +
  theme(legend.position = "bottom") +
  xlim(-3, 3) + ylim(0, 3)

=======

ModeleBifac$item

data.cqc01 %>% psych::tetrachoric() %>% .$rho %>% 
  psych::alpha()
>>>>>>> 665f3c56ba40ea550cb193404c9efaa34b45aba2:Analyse bifactorielle.R
