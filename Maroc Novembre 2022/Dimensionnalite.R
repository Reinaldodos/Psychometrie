Echelles = 
  FA$loadings %>% 
  unclass() %>% 
  data.frame() %>% 
  rownames_to_column(var = "item") %>% 
  gather(key = Dim, value = Loadings, -item) %>% 
  filter(Loadings > 0.3) %>% 
  split(x = .$item, f = .$Dim)


# Echelle ML1 -------------------------------------------------------------
Modele_ML1 =
  DATA %>% select(all_of(Echelles$ML1)) %>% 
  TAM::tam.mml.2pl()

#  ... insérer ici l'ensemble de l'analyse

Scores_ML1 = tam.wle(tamobj = Modele_ML1)

# Echele ML2 --------------------------------------------------------------

Modele_ML2 =
  DATA %>% select(all_of(Echelles$ML2)) %>% 
  TAM::tam.mml.2pl()

#  ... insérer ici l'ensemble de l'analyse

Scores_ML2 = tam.wle(tamobj = Modele_ML2)


# Deux scores par eleve ---------------------------------------------------
SCORES = 
  list(Scores_ML1,
     Scores_ML2) %>% 
  map(data.frame) %>% 
  reduce(.f = inner_join, by = "pid") %>% 
  select(pid, 
         theta_ML1 = theta.x,
         theta_ML2 = theta.y)

SCORES %>% 
  ggplot(mapping = aes(x = theta_ML1, y = theta_ML2))+
  geom_point()

SCORES %>% select(-pid) %>% cor() 

