require(icarus)

N <- 300 ## population total
## Horvitz Thompson estimator of the mean: 1.666667
weightedMean(data_employees$movies, data_employees$weight, N) 

## Enter calibration margins:
mar1 <- c("category",3,80,90,60)
mar2 <- c("sex",2,140,90,0)
mar3 <- c("department",2,100,130,0)
mar4 <- c("salary", 0, 470000,0,0)
margins <- rbind(mar1, mar2, mar3, mar4)
## Compute calibrated weights with raking ratio method

wCal_linear <-
  calibration(
    data = data_employees,
    marginMatrix = margins,
    colWeights = "weight",
    method = "linear"
  )

wCal_raking <-
  calibration(
    data = data_employees,
    marginMatrix = margins,
    colWeights = "weight",
    method = "raking"
  )

wCal_logit <-
  calibration(
    data = data_employees,
    marginMatrix = margins,
    colWeights = "weight",
    # bounds = c(0, 2.8),
    bounds = c(0.4, 1.8),
    calibTolerance = 1e-6,
    method = "logit"
  )

require(magrittr)
cbind.data.frame(Linear = wCal_linear, 
                 Raking = wCal_raking, 
                 Logit = wCal_logit) %>% 
  cor() %>% ggcorrplot::ggcorrplot(lab = T)

