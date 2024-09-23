source("R/Node.R")
source("R/Graph.R")
library(truncnorm)

#mother
maternal_age_node <- Node$new(name = "Maternal Age", func =
                                function(a, b, mean, sd)
                                  {round(rtruncnorm(1, a, b, mean, sd))},
                              args = list(15,45,30,5))

#father
paternal_age_node <- Node$new(name = "Paternal Age", func =
                                function(a, b, mean, sd)
                                  {round(rtruncnorm(1, a, b, mean, sd))},
                              args = list(15,50,33,5))

#child
child_sex_node <- Node$new(name = "Child Sex", func = function(prob_female = 0.5) {
  rbinom(1, 1, prob_female)
})

#outcome
simulate_outcome_logistic_regression <- function(maternal_age, paternal_age, child_sex,
                                                 beta_0, beta_maternal,
                                                 beta_paternal, beta_sex) {
  logit_p <- beta_0 + beta_maternal * maternal_age + beta_paternal * paternal_age + beta_sex * child_sex
  probability <- 1 / (1 + exp(-logit_p))
  outcome <- rbinom(1, size = 1, prob = probability)
  return(outcome)
}

child_ndd_node <- Node$new(name = "NDD presence", func =
                      simulate_outcome_logistic_regression,
                      args = list(maternal_age_node, paternal_age_node, child_sex_node,
                                  -5, 0.1, 0.08, 0.05))

epi_graph <- Graph$new(list(maternal_age_node, paternal_age_node,child_sex_node,
                            child_ndd_node), name = "EpiDAG")
output <- epi_graph$simulate(num_samples = 10, save = TRUE)
epi_graph$draw()
