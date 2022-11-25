require(tidyverse)
require(TAM)
require(CDM)

data("data.fims.Aus.Jpn.scored")
data("data.pisa00R.cc")

input =
  data.fims.Aus.Jpn.scored %>%
  select(starts_with("M1PT")) %>%
  select(-M1PTI21, -M1PTI14, -M1PTI12)

input %>% psych::describe()


# Comparaison des modèles -------------------------------------------------

Modele_1D = TAM::tam.mml(resp = input)
Modele_2D = TAM::tam.mml.2pl(resp = input)

CDM::IRT.compareModels(Modele_1D, Modele_2D)

# choix du modèle
Modele = Modele_2D


# Calcul du fit -----------------------------------------------------------
FIT = TAM::tam.modelfit(tamobj = Modele)

FIT %>% summary()

# fit global
Global_Fit = FIT$statlist$SRMSR <= 0.05

# fonction pour les dépendances locales
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

# matrice de corrélation entre les résidus: elle doit etre le plus blanc possible
FIT$residuals %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(hc.order = T)

# Graphiques des fit de chaque item
pdf(file = "Maroc Novembre 2022/fit des items.pdf")
plot(Modele)
dev.off()

# calcul des loadings: la qualité des items
CFA = TAM::IRT.linearCFA(object = Modele)
CFA$stand.loadings %>% view()


# on a validé:
# - fit global du modele 
# - pas d'indépendances locales
# - les items ont des graphiques acceptables
# - les loadings sont bons pour tous les items


# Suite de l'analyse psychométrique ---------------------------------------
Modele %>% TAM::IRT.WrightMap()

Scores = tam.wle(tamobj = Modele)

Scores %>% data.frame() %>% 
  count(theta, error) %>% 
  ggplot(mapping = aes(x = theta, y = error, size = n)) +
  geom_point()

Scores$theta %>% mean()
Scores$theta %>% sd()


