pacman::p_load(DAKS, CDM, psych, magrittr, tidyverse, TAM,
               ggraph)
data("data.fims.Aus.Jpn.scored")
# input = data.fims.Aus.Jpn.scored %>% dplyr::select(starts_with("M1"))
# input = DAKS::pisa
input = CDM::fraction.subtraction.data

# output = iita(dataset = input, v = 1)

# output %>% saveRDS(file = "IITA fraction.rds")
output = read_rds(file = "IITA fraction.rds")

hasse(imp = output$implications,
      items = ncol(input))

KST = imp2state(imp = output$implications,
                items = ncol(input))

require(kst)
KST = as.famset(m = KST,
                as.letters = FALSE)
KST = kst::kstructure(x = KST)
kst::kdomain(x = KST)

kst::knotions(x = KST)
KST_reduit = reduction(x = KST, operation = "discrimination")

kst::kstructure_is_kspace(KST_reduit)
kst::kstructure_is_wellgraded(KST_reduit)

KSP = kspace(x = KST_reduit)
Rgraphviz::plot(KSP)

kst::lpath_is_gradation(x = KSP)
kst::lpath(x = KSP)

