pacman::p_load(lavaan, tidySEM)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)

tidySEM::graph_sem(fit)
get_layout(fit)

tidySEM::graph_sem(
  fit,
  layout = get_layout(fit, 
                      layout_algorithm = "layout_on_grid"))

lay <- get_layout("", "", "visual","","textual","","speed","", "",
                  "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", 
                  rows = 2)

graph_sem(fit, layout = lay)


