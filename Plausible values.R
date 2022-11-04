pacman::p_load(magrittr, tidyverse, data.table, TAM, psych)

data("data.ex08")
DATA = data.ex08$resp

# Analyse dimensionnelle --------------------------------------------------
nfactors(x = DATA)

KOR = DATA %>% polychoric() %$% rho

KOR %>% fa.parallel(n.obs = nrow(DATA))

FANAL = KOR %>% fa(nfactors = 4, n.obs = nrow(DATA))
FANAL$crms
FANAL %>% fa.diagram(simple = F)

OMEGA =
  DATA %>%
  omega(
    nfactors = 4,
    poly = T,
    plot = F
  )

OMEGA %>% summary()
OMEGA %>% omega.diagram(cut = .3, sl = F, gcut = .3)
OMEGA %>% omega.diagram(cut = .3, sl = T, gcut = .3)

DATA %>%
  polychoric() %$% rho %>%
  fa(nfactors = 2) %>% fa.diagram()

DATA %>%
  polychoric() %$% rho %>%
  iclust()

# Modélisation ------------------------------------------------------------

Modele = DATA %>% tam.mml.2pl()

FIT = Modele %>% IRT.modelfit()
FIT %>% summary()

# Scoring -----------------------------------------------------------------
Scores = Modele %>% tam.wle() %>% data.table()

Scores %>%
  mutate(Variance = error ^ 2) %>%
  dplyr::summarise(M = mean(theta),
                   STD_Err = sqrt(mean(Variance) / n())) %>%
  mutate(M = 50 * M + 250,
         STD_Err = 50 * STD_Err)

Plausible_Values <- function(Modele, N_pv) {
  PV = Modele %>% tam.pv(nplausible = N_pv)
  output = 
    PV$pv %>%
    gather(key = PV, value = theta, starts_with("PV"))  
  return(output)
}

Confidence <- function(data, var) {
  var = enquo(var)
  output = 
  data %>% pull(!!var) %>% as.numeric() %>%
    Rmisc::CI(ci = .95) %>% broom::tidy() %>%
    spread(key = names, value = x) 
    return(output)
}

Scores_PV = Plausible_Values(Modele = Modele, N_pv = 10)

Moyennes =
  Plausible_Values(Modele = Modele, N_pv = 100) %>%
  mutate(Groupe = pid %% 2,
         theta = 50 * theta + 250) %>%
  group_by(PV) %>%
  dplyr::summarise(M = mean(theta),
                   SD = sd(theta)) %>%
  data.table()

Moyennes %>% Confidence(var = M) %>% data.table()
