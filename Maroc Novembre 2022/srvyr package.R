require(srvyr)

data(api, package = "survey")
mean(apipop$api00)
sum(apipop$enroll, na.rm = TRUE)

#stratified sample
dstrat <-
  apistrat %>% 
  as_survey_design(
    id =  1,
    strata =  stype,
    weights = pw,
    fpc = fpc
  )

summary(dstrat)

dstrat %>% 
  summarise(
    mean = survey_mean(api00, vartype = "se"),
    total = survey_total(enroll, vartype = "se"), 
  )

dstrat %>% 
  summarise(
    mean = survey_mean(api00, vartype = "var"),
    total = survey_total(enroll, vartype = "var"), 
  )

dstrat %>% 
  summarise(
    mean = survey_mean(api00, vartype = "cv"),
    total = survey_total(enroll, vartype = "cv"), 
  )

dstrat %>% 
  summarise(
    mean = survey_mean(api00, vartype = "ci", level = .8),
    total = survey_total(enroll, vartype = "ci", level = .95), 
  )

# group_by ----------------------------------------------------------------
strat_design_srvyr =
  apistrat %>%
  as_survey_design(ids = 1,
                   strata = stype,
                   fpc = fpc,
                   weight = pw,
                   variables = c(stype, starts_with("api")))

# on peut modifier la donnée à la volée
strat_design_srvyr =
  strat_design_srvyr %>%
  mutate(api_diff = api00 - api99) %>%
  rename(api_students = api.stu)


#  et on summarise en groupant

strat_design_srvyr %>%
  group_by(stype) %>%
  summarize(api_increase = survey_total(api_diff >= 0),
            api_decrease = survey_total(api_diff < 0))




