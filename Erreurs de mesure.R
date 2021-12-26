pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input =
  data.fims.Aus.Jpn.scored %>% select(starts_with("M")) %>%
  mutate_all(type.convert)



MOdele =
  input %>%
  TAM::tam.mml.2pl()

DELTA_EAP =
  MOdele$person %>%
  data.table() %>%
  mutate(Groupe = nchar(pid)) %>%
  group_by(Groupe) %>%
  mutate(DELTA = SD.EAP ^ 2 * (1 / n_distinct(pid)) ^ 2) %>%
  summarise(DELTA_EAP = sqrt(sum(DELTA)))

MOdele_PV = MOdele %>% tam.pv(nplausible = 10)
DELTA_PV =
  MOdele_PV$pv %>%
  gather(key = PV, value = score, -pid) %>%
  mutate(Groupe = nchar(pid)) %>%
  group_by(Groupe, PV) %>% summarise(M = mean(score)) %>%
  summarise(DELTA_PV = sd(M))


MOdele_theta = MOdele %>% tam.wle(WLE = T)
DELTA_theta =
  MOdele_theta %>%
  data.table() %>%
  mutate(Groupe = nchar(pid)) %>%
  group_by(Groupe) %>%
  mutate(DELTA = error ^ 2 * (1 / n_distinct(pid)) ^ 2) %>%
  summarise(DELTA = sqrt(sum(DELTA)))


DELTAs =
  list(DELTA_theta, DELTA_PV, DELTA_EAP) %>%
  reduce(.f = inner_join, by = "Groupe")


MOdele$person %>%
  distinct(pid) %>%
  count(Groupe = nchar(pid), name = "Nb_pid") %>%
  inner_join(y = DELTAs, by = "Groupe") %>%
  as_tibble()