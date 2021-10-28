require(tidyverse)
require(magrittr)

exemple = "exemple OCDE.xlsx"
input = exemple %>% rio::import()

require(lubridate)
Temps_bloc = 
  input %>% 
  summarise(T = dseconds(sum(Temps))/21) %>% pull(T)

input %>% 
  group_by(echelle) %>% 
  summarise(Duree = dseconds(sum(Temps))) %>% 
  mutate(Nb_blocs = round(Duree/Temps_bloc))

require(xxIRT)
pool =
  input %>%
  transmute(itxr16, id_situation, echelle, 
            Temps = dseconds(Temps),
            b = threshold, a = 1, c = 0) %>%
  split(f = .$echelle)

ATA = 
  pool$CE %>% 
  xxIRT::ata(num_form = 9, max_use = 1, group = "id_situation") 

ATA = ata_item_use(x = ATA, min = 1)

ATA = ata_constraint(x = ATA, 
                     min = Temps_bloc - dseconds(30), 
                     max = Temps_bloc + dseconds(30),
                     coef = "Temps")

pool$CE$b %>% quantile()

ATA = ata_obj_relative(x = ATA, mode = "max", coef = -0.2)

ATA = ATA %>% ata_solve()

Resultat = ATA$items %>% bind_rows() 
  
Resultat %>% 
  group_by(form) %>% 
  summarise(Duree = dseconds(sum(Temps)))

ATA %>% plot()
