pacman::p_load(sem)
input=sem::Tests
input %>% describe()

input %>% nfactors()
input %>% fa(nfactors = 2) %>% fa.diagram()
OMEGA=input %>% omega(nfactors = 2)
OMEGA %>% summary()

modele<-'
fra =~ +x1+x2+x3
math =~ +y1+y2+y3
'


lavaan::sem(model = modele, data = data.frame(scale(input))) %>% 
  lavaan::summary(fit.measures=T)

lavaan::cfa(model = OMEGA$model$lavaan, 
            data = data.frame(scale(input))) %>% 
  lavaan::summary()




require(lavaan)
## The famous Holzinger and Swineford (1939) example
HolzingerSwineford1939 %>% names
HS.model = 'visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
'

fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit, fit.measures = TRUE)

