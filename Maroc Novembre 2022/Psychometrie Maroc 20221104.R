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

Modele = 
  TAM::tam.mml.2pl(
  resp = Person_Item,       #     matrice personne item
  # Y = Contexte,           #     covariables
  # group = input$country,  #     modeles de groupe
  irtmodel = "2PL",         #     choix du modele
  # formulaY = ~ SEX * country, # regression latente
  pid = Eleves, #               # vecteur id eleves    
  pweights = Poids, #           # vecteur de poids
  verbose = TRUE, 
  control = list(increment.factor = 1.1))

# vous récupérez un objet de classe "TAM"
Modele %>% class()

# dont vous pouvez observer un résumé
Modele %>% summary()

# le score EAP est obtenu automatiquement
Modele$person$EAP %>% hist()

# les paramètres d'item aussi
Modele$item_irt

# et on peut observer la carte de Wright
pacman::p_load(WrightMap)
Modele %>% TAM::IRT.WrightMap()
