pacman::p_load(TAM, psych, tidyverse, data.table, GLDEX, rio)

input =
  "http://personality-project.org/r/datasets/psychometrics.prob2.txt" %>% 
  read.table(header=TRUE)

input %>% describe()
cor(input) %>% ggcorrplot::ggcorrplot(hc.order = T)

my.data = input %>% select(-ID) 

my.data %>% nfactors()
my.data %>% fa.parallel()
my.data %>% fa(nfactors = 2) %>% fa.diagram(simple = F)
my.data %>% fa(nfactors = 3) %>% fa.diagram(simple = F)

f2o <- fa(r = my.data[1:5], nfactors = 2)
f2e <- fa.extension(Roe = cor(my.data[1:5], my.data[6:8]), fo = f2o)
fa.diagram(
  fa.results = f2o,
  fe.result = f2e,
  simple = F,
  cut = .3,
  digits = 2
)


z.data <- data.frame(scale(my.data))
m1.model <-
'
ability =~ GREV + GREQ + GREA
motive =~  GREA + Ach +Anx
perform =~ Prelim + GPA + MA
'

m2.model <-
  '
ability =~ GREV + GREQ + GREA+ GPA + MA
motive =~  Ach + GREA + Prelim + Anx
'

Modele1 =
  lavaan::sem(
    m1.model,
    data = z.data
  )

Modele1 %>% lavaan::summary(fit.measures = T)

Modele1 %>% lavaan.diagram()

Modele2 =
  lavaan::sem(
    m2.model,
    data = z.data
  )

Modele2 %>% lavaan::summary(fit.measures = T)

Modele2 %>% lavaan.diagram()
