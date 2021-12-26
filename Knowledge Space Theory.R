pacman::p_load(DAKS, CDM, psych, magrittr, tidyverse, TAM,
               ggraph)
data("data.fims.Aus.Jpn.scored")
# input = data.fims.Aus.Jpn.scored %>% dplyr::select(starts_with("M1"))
# input = DAKS::pisa
input = CDM::fraction.subtraction.data

# output = iita(dataset = input, v = 1)

# output %>% saveRDS(file = "IITA fraction.rds")
output = read_rds(file = "IITA fraction.rds")

output$implications %>% hasse(items = ncol(input))
KST = output$implications %>% imp2state(items = ncol(input))

require(kst)
KST = KST %>% as.famset(as.letters = FALSE)
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

names(input) = 1:ncol(input)
