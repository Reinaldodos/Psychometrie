pacman::p_load(TAM, psych, tidyverse, data.table)

input = data.fims.Aus.Jpn.scored

# On regarde la qualité des données
input %>% psych::describe()

# On rajoute une colonne pour les id eleves, et des poids
require(magrittr)
input =
  input %>% 
  rowid_to_column(var = "id_eleve")

input$poids = runif(n = nrow(input), min = 0.9, 1.2)

Eleves = input$id_eleve
Poids = input$poids
Contexte = input %>% select(SEX, country)

# on écarte les 3 items "mauvais" (cf analyse dimensionnelle)
Person_Item = 
  input %>% 
  select(starts_with("M1")) %>%
  select(-M1PTI21, -M1PTI14, -M1PTI12)

# on modélise avec TAM

# Calibration concurrente -------------------------------------------------

Groupes = input$country %>% as.factor() %>% relevel(ref = "2")

Modele = 
  TAM::tam.mml.2pl(
    resp = Person_Item,       #     matrice personne item
    group = Groupes,  #     modeles de groupe
    irtmodel = "2PL",         #     choix du modele
    pid = Eleves, #               # vecteur id eleves    
    pweights = Poids, #           # vecteur de poids
    verbose = TRUE, 
    control = list(increment.factor = 1.1))

Modele %>% summary()

Scores = Modele %>% tam.wle()

cbind.data.frame(pid = Eleves, groupe = Groupes, poids = Poids) %>% 
  inner_join(y = data.frame(Scores), by = "pid") %>% 
  group_by(groupe) %>% 
  summarise(M = TAM::weighted_mean(x = theta, w = Poids),
            SD = TAM::weighted_sd(x = theta, w = Poids))


# Calibration mean-mean ---------------------------------------------------
require(magrittr)
input_split =
  input %>% 
  split(f = .$country)


Eleves_AUS = input_split$`1`$id_eleve
Poids_AUS = input_split$`1`$poids
Contexte_AUS = input_split$`1` %>% select(SEX, country)

# on écarte les 3 items "mauvais" (cf analyse dimensionnelle)
Person_Item_AUS = 
  input_split$`1` %>% 
  select(starts_with("M1")) %>%
  select(-M1PTI21, -M1PTI14, -M1PTI12)

Modele_AUS = 
  TAM::tam.mml.2pl(
    resp = Person_Item_AUS,       #     matrice personne item
    irtmodel = "2PL",         #     choix du modele
    pid = Eleves_AUS, #               # vecteur id eleves    
    pweights = Poids_AUS, #           # vecteur de poids
    verbose = TRUE, 
    control = list(increment.factor = 1.1))

Eleves_JPN = input_split$`2`$id_eleve
Poids_JPN = input_split$`2`$poids
Contexte_JPN = input_split$`2` %>% select(SEX, country)

# on écarte les 3 items "mauvais" (cf analyse dimensionnelle)
Person_Item_JPN = 
  input_split$`2` %>% 
  select(starts_with("M1")) %>%
  select(-M1PTI21, -M1PTI14, -M1PTI12)

Modele_JPN = 
  TAM::tam.mml.2pl(
    resp = Person_Item_JPN,       #     matrice personne item
    irtmodel = "2PL",         #     choix du modele
    pid = Eleves_JPN, #               # vecteur id eleves    
    pweights = Poids_JPN, #           # vecteur de poids
    verbose = TRUE, 
    control = list(increment.factor = 1.1))

inner_join(x = Modele_AUS$item_irt,
           y = Modele_JPN$item_irt,
           by = "item") %>% 
  ggplot(mapping = aes(x = beta.x, y = beta.y)) +
  geom_point() +
  geom_abline()

inner_join(x = Modele_AUS$item_irt,
           y = Modele_JPN$item_irt,
           by = "item") %>% 
  ggplot(mapping = aes(x = alpha.x, y = alpha.y)) +
  geom_point() +
  geom_abline()


# Calibration par méthode des courbes d'info -------------------------------------
LINK = 
  TAM::tam.linking(
  tamobj_list = list(Modele_AUS, Modele_JPN), 
  type = "Hae", method = "chain"
  )

LINK$parameters_list

