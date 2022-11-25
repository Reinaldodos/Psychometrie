require(tidyverse)
require(TAM)
require(CDM)

data("data.fims.Aus.Jpn.scored")
data("data.pisa00R.cc")

input = 
  data.pisa00R.ct$data %>% 
  select(starts_with("R"))

Poids = data.pisa00R.ct$data$W_FSTUWT

Items_poly = 
  input %>% psych::describe() %>% 
  filter(range>1) %>% rownames()

input = input %>% select(-all_of(Items_poly))

input = input %>% select(-R216Q06)

KOR = 
  input %>% 
  psych::polychoric()

KOR$rho %>% ggcorrplot::ggcorrplot()

Modele_1D = TAM::tam.mml(resp = input, pweights = Poids)
Modele_2D = TAM::tam.mml.2pl(resp = input, pweights = Poids)

CDM::IRT.compareModels(Modele_1D, Modele_2D)

Modele = Modele_2D

FIT = TAM::tam.modelfit(tamobj = Modele)

FIT %>% summary()
Global_Fit = FIT$statlist$SRMSR <= 0.05

Graphique_dependances <- function(FIT, SEUIL = 0.2) {
  Dependances = 
    FIT$stat.itempair %>% 
    filter(abs(aQ3) > SEUIL)
  
  require(tidygraph)
  require(igraph)
  require(ggraph)
  Graphe_dependances = 
    Dependances %>% 
    as_tbl_graph(directed = FALSE)
  
  Plot_dependances = 
    Graphe_dependances %>% 
    ggraph(layout = "graphopt") +
    geom_edge_link() +
    geom_node_text(mapping = aes(label = name), 
                   repel = TRUE)
  
  return(Plot_dependances)
}

Graphique_dependances(FIT = FIT, SEUIL = 0.2)
FIT$stat.itempair %>% view()

FIT$residuals %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(hc.order = T)

# Graphiques des fit de chaque item
pdf(file = "Maroc Novembre 2022/fit des items.pdf")
plot(Modele)
dev.off()

# FIT$residuals %>% 
#   data.frame() %>% 
#   summarise_all(.funs = ~ mean(.^2))

CFA = TAM::IRT.linearCFA(object = Modele)
CFA$stand.loadings %>% view()


# on a validé:
# - fit global du modele 
# - pas d'indépendances locales
# - les items ont des graphiques acceptables
# - les loadings sont bons pour tous les items

Modele %>% TAM::IRT.WrightMap()

Scores = tam.wle(tamobj = Modele)

Scores %>% data.frame() %>% 
  count(theta, error) %>% 
  ggplot(mapping = aes(x = theta, y = error, size = n)) +
  geom_point()



