require(tidyverse)
require(psych)
input = CDM::fraction.subtraction.data
QMAT = CDM::fraction.subtraction.qmatrix

din_identifiability(q.matrix = QMAT) %>% summary()

fractions.dina =
  din(data = input,
      q.matrix = QMAT,
      rule = "DINA")

fractions.dino =
  din(data = input,
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


CDM::IRT.compareModels(fractions.dina,
                       fractions.dino,
                       fractions.gdina,
                       fractions.acdm,
                       fractions.rrum)

choix = fractions.acdm
modelfit.cor.din(dinobj = choix) %>% summary()

CDM::plot_item_mastery(object = choix)

plot(choix, ask = F)

CDM::skill.cor(object = choix) %$% cor.skills %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, lab = T)
