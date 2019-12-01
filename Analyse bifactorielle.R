pacman::p_load(TAM, psych, tidyverse, data.table, mirt)


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
  pivot_longer(cols = -item, names_to = "Factor", values_to = "loadings") %>% 
  filter(abs(loadings)>.3) %>% 
  pivot_wider(names_from = "Factor", values_from = "loadings") %>% 
  drop_na("g") %>% select(-g)

Dims = 
  Items_high_order %>%
  pivot_longer(
    cols = -item,
    names_to = "Factor",
    values_to = "loadings",
    values_drop_na = T
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
  filter(p.holm<1) %>% 
  inner_join(x=FIT$stat.itempair,
             by = c("index1", "index2"))

IRT.compareModels(ModeleBifac, Modele_Uni)

ModeleBifac$item
