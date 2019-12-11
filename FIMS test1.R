FitPlot <- function(model, data, ...) {
  Scores = model %>% tam.wle() %>% data.table()
  
  Param = model$item_irt %>% as.list()
  names(Param$alpha) = Param$item
  names(Param$beta) = Param$item
  
  pacman::p_load(xxIRT)
  xxIRT::model_3pl_fitplot(
    u = data %>% as.matrix(),
    t = Scores$theta,
    a = Param$alpha,
    b = Param$beta,
    c = 0,
    D = 1.7, ...
  ) %>%
    return()
}

pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input = 
  data.fims.Aus.Jpn.scored %>% select(starts_with("M")) 


# Dimensionnalité/validité --------------------------------------------------

TET = input %>% psych::polychoric() %>% .$rho
TET %>% ggcorrplot::ggcorrplot(hc.order = T)
TET %>% fa.parallel(n.obs = 6371)

TET %>% fa(nfactors = 5) %>% fa.diagram(cut = .3)

OMEGA = TET %>% omega(nfactors = 5, plot = F) 
OMEGA %>% summary()
OMEGA %>% omega.diagram(simple = T, cut = .3, gcut = .3, digits = 2)

input =
  input %>%
  select(-M1PTI12,-M1PTI14,-M1PTI21) 

input %>% fa.parallel(cor = "poly")  

input %>% omegaSem(poly = T, nfactors = 2) %>% summary()

input %>% names
Dims = c(1, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1)

keys.list =
  list(
    all = c(
      "M1PTI1",
      "M1PTI2",
      "M1PTI3" ,
      "M1PTI6" ,
      "M1PTI7",
      "M1PTI11",
      "M1PTI17",
      "M1PTI18",
      "M1PTI19",
      "M1PTI22" ,
      "M1PTI23"
    ),
    Dim1 = c(
      "M1PTI1",
      "M1PTI2",
      "M1PTI3" ,
      "M1PTI6" ,
      "M1PTI11",
      "M1PTI18",
      "M1PTI23"
    ),
    Dim2 = c(
      "M1PTI7",
      "M1PTI17",
      "M1PTI19",
      "M1PTI22" 
    )
  )

unidim(x = input, keys.list = keys.list)
# TCT: internal consistency -----------------------------------------------
input %>% psych::polychoric() %>% .$rho %>%  psych::alpha()

# TCT: sensibilité --------------------------------------------------------
input %>% psych::describe()

input %>% rowSums() %>% hist()
input %>% rowSums() %>% GLDEX::fun.moments.r(normalise = "Y")

Modeles = list("Rasch" = tam.mml,
               "2PL" = tam.mml.2pl,
               "3PL"  = tam.mml.3pl)

output = invoke_map(.f = Modeles, resp = input, verbose = F)

IRT.compareModels(output$Rasch, output$`2PL`, output$`3PL`)

outputFIT = map(.x = output, .f = IRT.modelfit)
outputFIT %>% map(summary)


outputFIT %>% map(.f = ~ .$stat.itempair %>% filter(abs(aQ3) > .2))

output$`2PL` %>% FitPlot(data = input)

outputFIT$`2PL`$chisquare.itemfit

xxIRT::model_3pl_plot(
  a = output$`2PL`$item_irt$alpha,
  b = output$`2PL`$item_irt$beta,
  c = 0,
  D = 1.7, 
  total = "T",
  type = "info"
) +
  geom_hline(yintercept = 2)


# SCORING -----------------------------------------------------------------

Scores = output$`2PL` %>% tam.wle() %>% data.table()

Scores %>% count(theta, error) %>% 
  ggplot(mapping = aes(x = theta, y = error, size = n)) +
  geom_point()

data.fims.Aus.Jpn.scored %>%
  select(-starts_with("M1")) %>%
  rowid_to_column(var = "pid") %>%
  inner_join(x = Scores) %>%
  ggplot(mapping = aes(y = theta, 
                       x = factor(country), 
                       fill=factor(SEX))) +
  geom_boxplot() +
  coord_flip()


# Plausible values --------------------------------------------------------
Plausible_Values <- function(Modele, N_pv) {
  PV = Modele %>% tam.pv(nplausible = N_pv)
  PV$pv %>% 
    gather(key = PV, value = theta, starts_with("PV")) %>%
    return()
}

Confidence <- function(data, var) {
  var = enquo(var)
  data %>% pull(!!var) %>% as.numeric() %>%
    Rmisc::CI(ci = .95) %>% broom::tidy() %>%
    spread(key = names, value = x) %>% 
    return()
}

PV = Plausible_Values(Modele = output$`2PL`, N_pv = 100)
  
data.fims.Aus.Jpn.scored %>%
  select(-starts_with("M1")) %>%
  rowid_to_column(var = "pid") %>%
  inner_join(x = PV) %>%
  group_by(SEX, country, PV) %>%
  summarise(score = median(theta)) %>% ungroup %>% 
  group_by(SEX, country) %>% nest %>%
  mutate(CI = map(.x = data, .f = Confidence, var = score)) %>%
  select(-data) %>% unnest %>% data.table() %>% 
  ggplot(mapping = aes(
    ymin = lower,
    ymax = upper,
    x = factor(country),
    colour = factor(SEX)
  )) +
  geom_linerange(position = position_dodge(width = .1)) +
  coord_flip()


data.fims.Aus.Jpn.scored %>%
  select(-starts_with("M1")) %>%
  rowid_to_column(var = "pid") %>%
  inner_join(x = Scores) %>%
  mutate(VAR = error ^ 2) %>%
  group_by(SEX, country) %>%
  summarise(M = mean(theta),
            SE = sqrt(mean(VAR) / n())) %>% ungroup %>%
  mutate(lower = M + qnorm(.05 / 2) * SE,
         upper = M - qnorm(.05 / 2) * SE) %>%
  mutate_at(.vars = vars(SEX, country), .funs = as.factor) %>%
  mutate_if(.predicate = is.numeric, .funs = ~250+50*.) %>% 
  ggplot(mapping = aes(
    ymin = lower,
    ymax = upper,
    x = country,
    colour = SEX
  )) +
  geom_linerange(position = position_dodge(width = .1)) +
  coord_flip()

