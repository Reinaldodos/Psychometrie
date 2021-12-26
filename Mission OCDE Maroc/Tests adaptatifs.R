require(tidyverse)
require(magrittr)

get_pool <- function(seq) {
  seq %>%
    enframe(name = "item", value = "b") %>%
    mutate(a = 1,
           c = 0,
           item = str_c("item", item)) %>%
    return()
}

input = seq(-2, 2, 1) %>% get_pool()

require(xxIRT)

xxIRT::model_3pl_plot(a = input$a, b = input$b, c = 0)

xxIRT::model_3pl_plot(a = input$a,
                      b = input$b,
                      c = 0,
                      type = "info") 

xxIRT::model_3pl_info(
  a = input$a,
  b = input$b,
  c = 0,
  t = seq(-4, 4, .5)) %>% 
  view()

# CAT ---------------------------------------------------------------------
output = runif(n = 400, min = -2, max = 2) %>% get_pool()

CAT =
  rnorm(n = 1) %>%
  xxIRT::cat_sim(
    pool = output,
    min = 1,
    max = 50,
    stop_se = .2
  ) 

CAT %>% plot() 

CUT = qnorm(p = 1 / 3)
CAT =
  rnorm(n = 1) %>%
  cat_sim(
    pool = output,
    min = 1, max = 400,
    stop_cut = CUT
  ) 

CAT %>%
  plot() +
  geom_hline(yintercept = CUT, linetype = "dotted")


# MST ---------------------------------------------------------------------
MST = 
  xxIRT::mst(
    pool = output,
    design = "1-2",
    num_panel = 1,
    method = "bottomup", 
    max_use = 1, len = 10
  )


MST = mst_obj(x = MST, theta = 0, indices = 1)
MST = mst_obj(x = MST, theta = -1, indices = 2)
MST = mst_obj(x = MST, theta = 1, indices = 3)

MST = mst_assemble(x = MST)

MST %>% plot(byroute = T)
MST %>% plot(byroute = F)

MST$items %>% split(f = .$module)


# ATA ---------------------------------------------------------------------

ATA = ata(pool = output, num_form = 13, max_use = 1, len = 10) 
ATA = xxIRT::ata_obj_relative(x = ATA, coef = 0, mode = "max")

ATA %>% ata_solve() %>% plot()
