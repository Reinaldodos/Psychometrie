pacman::p_load(TAM, psych, tidyverse, xxIRT)

data("data.fims.Aus.Jpn.scored", package = "TAM")
DATA = data.fims.Aus.Jpn.scored %>% select(starts_with("M")) %>% 
  select(-M1PTI21, -M1PTI12, -M1PTI14)
DATA %>% psych::describe()

TAM =
  DATA %>%
  TAM::tam.mml.2pl()

TAM %>% tam.modelfit() %>% summary()

Scores = tam.wle(tamobj = TAM)

require(xxIRT)
pool =
  TAM$item_irt %>%
  transmute(item,
            a = alpha,
            b = beta,
            c = 0,
            D = 1)

CAT =
  Scores$theta %>%
  map(
    .f = cat_sim,
    pool = pool,
    min = 1,
    max = nrow(pool),
    stop_se = 1 / sqrt(5),
    info_random = 5
  )

DATA_sim =
  CAT %>%
  map(.f = ~ .$admin) %>%
  map(.f = data.table::data.table) %>%
  bind_rows(.id = "taker") %>%
  select(taker, item, u) %>%
  spread(item, u) %>% select(-taker)

KORsim =
  DATA_sim %>%
  select(names(DATA)) %>%
  psych::polychoric() %>%
  .$rho %>%
  ggcorrplot::ggcorrplot(hc.order = F)

KOR =
  DATA %>%
  psych::polychoric() %>%
  .$rho %>%
  ggcorrplot::ggcorrplot(hc.order = F)

# DATA_sim %>% psych::describe() %>% view()
require(patchwork)

KOR + KORsim

TAM_sim = 
  DATA_sim %>%
  TAM::tam.mml.2pl()

TAM_sim %>% tam.modelfit() %>% summary()

list(TAM$item_irt,
     TAM_sim$item_irt) %>%
  reduce(.f = inner_join, by = "item") %>% 
  # ggplot(mapping = aes(x = alpha.x, y = alpha.y))+
  ggplot(mapping = aes(x = beta.x, y = beta.y)) +
  geom_point() +
  geom_abline()+
  geom_smooth()


list(TAM,
     TAM_sim) %>%
  map(.f = TAM::IRT.linearCFA) %>% 
  map(.f = ~.$stand.loadings) %>% 
  reduce(.f = inner_join, by = "item") %>% 
  ggplot(mapping = aes(x = stand.load.Dim1.x, y = stand.load.Dim1.y)) +
  geom_point() +
  geom_abline()+
  geom_smooth()

