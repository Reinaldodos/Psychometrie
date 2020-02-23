# Load TAM library
pacman::p_load(rio, tidyverse, data.table, TAM, psych)
data("data.cqc05")

input = data.cqc05 %>% select(starts_with("A"))
facets = data.cqc05 %>% select(-starts_with("A")) %>% data.table()

formulaA <-  ~ item * (gender + level + gbyl)

mod2 <-
  tam.mml.mfr(
    resp = input,
    facets = as.data.frame(facets),
    formulaA = formulaA,
    irtmodel = "2PL"
  )

mod1 <-
  tam.mml(
    resp = input,
    irtmodel = "2PL"
  )

SAFE =
  data.cqc05 %>%
  gather(key = item,
         value = code,
         starts_with("A"),
         na.rm = T) %>%
  count(gender, level, gbyl, item) %>%
  filter(n >= 400) %>%
  spread(key = item, value = n) %>% drop_na() %>% names

DATA = data.cqc05 %>% select(SAFE)

input = DATA %>% select(starts_with("A"))
facets = DATA %>% select(-starts_with("A")) %>% data.table()

formulaA <-  ~ item * (gender + level + gbyl)

mod2 <-
  tam.mml.mfr(
    resp = input,
    facets = as.data.frame(facets),
    formulaA = formulaA,
    irtmodel = "2PL"
  )
mod2$xsi
mod2$xsi.facets %>%
  mutate(z = xsi / se.xsi) %>%
  filter(str_detect(string = facet, pattern = "item:")) %>%
  tidyr::extract(col = parameter,
                 into = c("item", "selon"),
                 regex = "(.*):(.*)") %>%
  mutate(p.value = 2 * pnorm(q = -abs(z))) %>%
  mutate(CI = 1 - p.value) %>%
  filter(CI > .95) %>%
  group_by(item, facet) %>%
  summarise(DIF=max(xsi)-min(xsi)) %>%
  data.table() %>% arrange(desc(DIF))
