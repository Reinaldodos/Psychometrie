pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input = data.fims.Aus.Jpn.scored%>% select(starts_with("M")) %>% 
  select(-M1PTI12, -M1PTI14, -M1PTI21) 

contexte = data.fims.Aus.Jpn.scored %>% select(-starts_with("M")) %>% 
  mutate_all(.funs = as.factor)

DIF =
  tam.mml.mfr(
    resp = input,
    formulaA =  ~ item * (SEX + country),
    facets = contexte
  )

DIFS = 
  DIF$xsi.facets %>%
  filter(str_detect(string = facet, pattern = "item:")) %>%
  mutate(zscore = xsi / se.xsi) %>%
  mutate(p.value = pnorm(q = -abs(zscore))) %>%
  tidyr::extract(col = parameter,
                 into = c("item", "selon"),
                 regex = "(.*):(.*)") %>%
  filter(p.value < 0.05) %>% 
  group_by(item, facet) %>% 
  summarise(DIF = max(xsi)-min(xsi)) %>% ungroup 

DIFS