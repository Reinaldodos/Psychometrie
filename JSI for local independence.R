Fetch_alpha <- function(model) {
  require(dplyr)
  inner_join(
    cbind.data.frame(item = rownames(model$B), model$B),
    cbind.data.frame(item = rownames(model$se.B), model$se.B),
    by = "item"
  ) %>%
    select(item, alpha = Cat1.Dim01.x, se.alpha = Cat1.Dim01.y)
}
Param_sans_item <- function(input, item) {
  require(TAM)
  require(dplyr)
  print(item)
  input %>% select(-item) %>%
    tam.mml.2pl(verbose = F) %>%
    Fetch_alpha() %>%
    return()
}
FOLD <- function(GRAPH) {
  require(igraph)
  graph.data.frame(GRAPH, directed = TRUE) %>%
    as.undirected(mode = "collapse", edge.attr.comb = "sum") %>%
    return()
}
JSI <- function(input) {
  require(TAM)
  require(tidyverse)
  Items = names(input)
  Modele_2PL = input %>% TAM::tam.mml.2pl()
  
  Jack =
    Items %>% set_names() %>%
    map(.f = Param_sans_item, input = input)
  
  JSI =
    Jack %>% bind_rows(.id = "item retiré") %>%
    left_join(x = Modele_2PL %>% Fetch_alpha(), by = "item") %>%
    mutate(D = (alpha.x - alpha.y) - se.alpha.y) %>%
    select(item, `item retiré`, D)
  
  list(Modele = Modele_2PL, JSI=JSI) %>% 
    return()
}
FOLDING <- function(JSI) {
  require(tidygraph)
  require(igraph)
  undirected_graph =
    JSI %>%
    FOLD()
  
  res <- data.frame(get.edgelist(undirected_graph),
                    get.edge.attribute(undirected_graph, "D"))
  colnames(res) <- c("item1", "item2", "JSI")
  return(res)
}

pacman::p_load(TAM, psych, tidyverse, data.table)
data("data.fims.Aus.Jpn.scored")
input = 
  data.fims.Aus.Jpn.scored %>% select(starts_with("M")) %>%
  select(-M1PTI21, -M1PTI14, -M1PTI12)
input %>% psych::describe()


output = JSI(input = input)

output$JSI %>%
  ggplot(mapping = aes(x = item, y = `item retiré`, colour = D)) +
  geom_point() +
  scale_colour_gradient2(low = "green", high = "red")

FOLDING(JSI = output$JSI) %>%
  ggplot(mapping = aes(x = item1, y = item2, colour = JSI)) +
  geom_point() +
  scale_colour_gradient2(low = "green", high = "red")

Modele_2PL_FIT = output$Modele %>% IRT.modelfit()
Modele_2PL_FIT %>% summary()
Modele_2PL_FIT$stat.itempair %>% filter(abs(aQ3) > .2)

