pacman::p_load(TAM, psych, tidyverse, data.table, mirt)

data("data.cqc01")

Modele = tam.fa(resp = data.cqc01,
                irtmodel = "efa",
                nfactors = 2, 
                control = list(increment.factor=1.2))

FIT = Modele %>% IRT.modelfit()
FIT %>% summary()
Modele$meas

Modele$efa.oblimin$loadings %>% fa.diagram(cut = .3)
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
  data.cqc01 %>%
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
  data.cqc01 %>%
  select(Items_high_order$item %>% as.character()) %>%
  tam.mml.2pl(irtmodel = "2PL",
              control = list(increment.factor = 1.2))

FIT=Modele_Uni %>% IRT.modelfit() 
FIT %>% summary()
FIT$chi2.stat %>%
  filter(p.holm < 1) %>%
  inner_join(x = FIT$stat.itempair,
             by = c("index1", "index2"))

IRT.compareModels(ModeleBifac, Modele_Uni, Modele)

ModeleBifac$item

data.cqc01 %>% psych::tetrachoric() %>% .$rho %>% 
  psych::alpha()
