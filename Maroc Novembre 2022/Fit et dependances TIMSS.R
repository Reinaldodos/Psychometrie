require(tidyverse)
require(TAM)
require(CDM)

data("data.timss11.G4.sa")

input = 
  data.timss11.G4.AUT$data %>% 
  select(starts_with("M"))


Poids = data.timss11.G4.AUT$data$TOTWGT


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

FIT$stat.itempair %>% view()
Graphique_dependances(FIT = FIT, SEUIL = 0.2)

FIT$residuals %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(hc.order = F)


# trop de dépendances locales: il faut y remédier
# - soit exclure une partie des items en dépendances
# - regrouper les items MAIS ATTENTION on ne peut regrouper que les items d'un meme bloc

# on garde pour chaque groupe le meilleur item
Dependances = 
  FIT$stat.itempair %>% 
  filter(abs(aQ3) > 0.2)

require(tidygraph)
require(igraph)
require(ggraph)
Graphe_dependances = 
  Dependances %>% 
  as_tbl_graph(directed = FALSE)

Dependances_table = 
  Graphe_dependances %>% 
  igraph::decompose.graph() %>% 
  map(.f = igraph::get.vertex.attribute, name = "name") %>% 
  map(.f = ~data.frame(item = .)) %>% 
  bind_rows(.id = "Groupe")

CFA = TAM::IRT.linearCFA(object = Modele)
Best_of = 
  CFA$stand.loadings %>% 
  inner_join(y = Dependances_table, by = "item") %>% 
  group_by(Groupe) %>% 
  top_n(n = 1, wt = stand.load.Dim1) %>% ungroup()

Items_a_ecarter = 
  anti_join(x = Dependances_table,
            y = Best_of, by = "item") %>% 
  pull(item)

input = input %>% select(-all_of(Items_a_ecarter))



# On reprend depuis le début ----------------------------------------------
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

Graphique_dependances(FIT = FIT, SEUIL = 0.2)
FIT$stat.itempair %>% view()

FIT$residuals %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(hc.order = F)


# Graphiques des fit de chaque item
pdf(file = "Maroc Novembre 2022/fit des items.pdf")
plot(Modele)
dev.off()


CFA = TAM::IRT.linearCFA(object = Modele)
Items_a_ecarter = 
  CFA$stand.loadings %>% 
  filter(stand.load.Dim1<0.2) %>% pull(item)

input = input %>% select(-all_of(Items_a_ecarter))



# On reprend depuis le début ----------------------------------------------
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

Graphique_dependances(FIT = FIT, SEUIL = 0.2)
FIT$stat.itempair %>% view()

FIT$residuals %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot::ggcorrplot(hc.order = F)


# Graphiques des fit de chaque item
pdf(file = "Maroc Novembre 2022/fit des items.pdf")
plot(Modele)
dev.off()

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
  ggplot(mapping = aes(x = theta, 
                       y = error, 
                       size = n)) +
  geom_point()


# Et si on n'avait rien vérifié? ------------------------------------------
Modele_non_verifie = 
  data.timss11.G4.AUT$data %>% 
  select(starts_with("M")) %>% 
  TAM::tam.mml.2pl()

Scores_non_verifies = tam.wle(tamobj = Modele_non_verifie)

Modeles_comp = 
  list(Modele$item_irt,
     Modele_non_verifie$item_irt) %>% 
  reduce(.f = inner_join, by = "item")

Modeles_comp %>% 
  ggplot(mapping = aes(x = beta.x, y = beta.y)) +
  geom_point() + 
  geom_abline() +
  geom_smooth()


Modeles_comp %>% 
  ggplot(mapping = aes(x = alpha.x, 
                       y = alpha.y)) +
  geom_point() + 
  geom_abline() +
  geom_smooth()

Get_loadings <- function(Modele) {
  require(magrittr)
  Loadings = Modele %>% TAM::IRT.linearCFA() %$% stand.loadings
  return(Loadings)
}


list(Modele,
     Modele_non_verifie) %>% 
  map(.f = Get_loadings) %>% 
  reduce(.f = inner_join, by = "item") %>% 
  ggplot(mapping = aes(x = stand.load.Dim1.x, 
                       y = stand.load.Dim1.y)) +
  geom_point() + geom_abline() +
  geom_smooth()

list(Scores, Scores_non_verifies) %>% 
  map(.f = data.frame) %>% 
  reduce(.f = inner_join, by = "pid") %>% 
  ggplot(mapping = aes(x = theta.x, 
                       y = theta.y)) +
  geom_point() + geom_abline() +
  geom_smooth()

