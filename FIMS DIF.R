pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX)
data("data.fims.Aus.Jpn.scored")
input = 
  data.fims.Aus.Jpn.scored %>% select(starts_with("M"))
contexte = data.fims.Aus.Jpn.scored %>% 
  select(-starts_with("M")) %>% 
  mutate_all(.funs = as.factor)

DIF =
  tam.mml.mfr(
    resp = input,
    formulaA =  ~ item * (SEX + country),
    facets = contexte, 
    verbose = F
  )

DIFS = 
  DIF$xsi.facets %>%
  filter(str_detect(string = facet, pattern = "item:")) %>%
  mutate(zscore = xsi / se.xsi) %>%
  mutate(p.value = pnorm(q = -abs(zscore))) %>%
  tidyr::extract(col = parameter,
                 into = c("item", "selon"),
                 regex = "(.*):(.*)") 

DIFS %>%
  ggplot(mapping = aes(x = item, colour = selon)) +
  geom_linerange(
    mapping = aes(ymin = xsi - 1.96 * se.xsi,
                  ymax = xsi + 1.96 * se.xsi),
    position = position_dodge(width = .2)
  ) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ facet)


DIFS %>%
  group_by(item, facet) %>%
  summarise(DIF = max(xsi) - min(xsi),
            DIF_error = sqrt(sum(se.xsi^2))) %>% 
  ggplot(mapping = aes(y = item, colour = facet)) +
  geom_linerange(
    mapping = aes(xmin = DIF - 1.96 * DIF_error,
                  xmax = DIF + 1.96 * DIF_error),
    position = position_dodge(width = .2)
  ) +
  geom_vline(xintercept = c(0,.5))



