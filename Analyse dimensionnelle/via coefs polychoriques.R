pacman::p_load(TAM, psych, tidyverse, data.table)
data("data.fims.Aus.Jpn.scored", package = "TAM")
DATA = data.fims.Aus.Jpn.scored %>% select(starts_with("M1"))
TET = DATA %>% psych::polychoric()

TET$rho %>% ggcorrplot::ggcorrplot(hc.order = T)

TET$rho %>% fa.parallel(n.obs = nrow(DATA), fa = "fa")


NFACTORS = 
  TET$rho %>% psych::nfactors(n = 5, rotate = "oblimin", 
                              n.obs = nrow(DATA))


FA = TET$rho %>% psych::fa(nfactors = 1,
                           n.obs = nrow(DATA),
                           rotate = "oblimin",
                           fm = "ml")

FA %>% fa.diagram(cut = .3, sort = T, simple = F)
FA$Structure

Items_EFA = 
  FA$loadings %>% unclass() %>%
  data.frame() %>% rownames_to_column(var = "item") %>%
  gather(key = Dim, value = Loading,-item) %>%
  filter(abs(Loading) > .3) %>% distinct(item) %>% droplevels()

TET_EFA = DATA %>% select(Items_EFA$item) %>% psych::polychoric()

TET_EFA$rho %>% fa.parallel(n.obs = nrow(DATA), fa = "fa")
FA = TET_EFA$rho %>% psych::fa(nfactors = 3,
                           n.obs = nrow(DATA),
                           fm = "ml")
FA %>% fa.diagram(cut = .3, sort = T)
FA$Structure

OMEGA =
  TET_EFA$rho %>% psych::omega(
    nfactors = 3,
    n.obs = nrow(DATA),
    fm = "ml",
    plot = F
  )
OMEGA %>% summary()
OMEGA%>% omega.diagram(cut = .3, gcut = .3, sort = T)
OMEGA%>% omega.diagram(cut = .3, gcut = .3, sl = T)

OMEGA$omega.group %>% rownames_to_column(var = "Dim") %>%
  group_by(Dim) %>% transmute(general / total, group / total)

TET$rho %>% psych::iclust()
