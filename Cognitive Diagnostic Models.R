require(tidyverse)
require(CDM)

input = CDM::fraction.subtraction.data
QMAT = CDM::fraction.subtraction.qmatrix

Modele_IRT = TAM::tam.mml.2pl(resp = input)

QMAT %>% as.matrix() %>% image()

din_identifiability(q.matrix = QMAT) %>% summary()

fractions.dina = din(data = input,
                     q.matrix = QMAT,
                     rule = "DINA")

fractions.dino = din(data = input,
                     q.matrix = QMAT,
                     rule = "DINO")

fractions.acdm <- gdina(input,
                        QMAT,
                        rule = "ACDM", 
                        HOGDINA = T)

fractions.rrum <- gdina(input,
                        QMAT,
                        rule = "RRUM", 
                        HOGDINA = T)

fractions.gdina <- gdina(input,
                         QMAT,
                         rule = "GDINA", 
                         HOGDINA = T)


CDM::IRT.compareModels(Modele_IRT,
                       fractions.dina,
                       fractions.dino,
                       fractions.gdina,
                       fractions.acdm,
                       fractions.rrum)


choix = fractions.rrum

CDM::modelfit.cor.din(dinobj = choix) %>% summary()

FIT = CDM::modelfit.cor.din(dinobj = choix) 
FIT$itempairs %>% filter(abs(Q3) > 0.2)

CDM::plot_item_mastery(object = choix)

plot(choix, ask = TRUE)

choix$item %>% view()
choix$skill.patt
choix$subj.pattern %>% count(pattern)
choix$pattern %>% view()

CDM::skill.cor(object = choix) %$% cor.skills %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, lab = T)

