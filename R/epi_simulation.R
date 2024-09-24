source("R/Node.R")
source("R/Graph.R")
library(truncnorm)


#mother
maternal_age_node <- Node$new(name = "Maternal age", func =
                                function(a, b, mean, sd)
                                  {round(rtruncnorm(1, a, b, mean, sd))},
                              args = list(15,45,30,5))

maternal_mental_disorder_node <- Node$new("Maternal mental disorder",
                                          func = function(prob_mental = 0.39) {
                                            rbinom(1, 1, prob_mental)
                                          })

maternal_substance_abuse_node <- Node$new("Maternal substance abuse",
                                          func = function(prob_abuse = 0.56) {
                                            rbinom(1, 1, prob_abuse)
                                          })

#father
simulate_valporate_logistic_regression <- function(paternal_age,
                                                 paternal_mental_disorder,
                                                 beta_0,
                                                 beta_paternal_age,
                                                 beta_paternal_mental_disorder) {
  logit_p <- beta_0 + beta_paternal_age * paternal_age +
    beta_paternal_mental_disorder * paternal_mental_disorder
  probability <- 1 / (1 + exp(-logit_p))
  outcome <- rbinom(1, size = 1, prob = probability)
  return(outcome)
}

paternal_age_node <- Node$new(name = "Paternal age", func =
                                function(a, b, mean, sd)
                                  {round(rtruncnorm(1, a, b, mean, sd))},
                              args = list(15,50,33,5))

paternal_mental_disorder_node <- Node$new("Paternal mental disorder",
                                          func = function(prob_female = 0.25) {
  rbinom(1, 1, prob_female)
})

paternal_substance_abuse_node <- Node$new("Paternal substance abuse",
                                          func = function(prob_abuse = 0.42) {
                                            rbinom(1, 1, prob_abuse)
                                          })

paternal_valporate_use_node <- Node$new("Paternal valporate use",
                                        func = simulate_valporate_logistic_regression,
                                        args = list(paternal_age_node,
                                                    paternal_mental_disorder_node,
                                                    -5, 0.1, 0.08))

#child
child_sex_node <- Node$new(name = "Child sex", func = function(prob_female = 0.5) {
  rbinom(1, 1, prob_female)
})

#outcome
simulate_outcome_logistic_regression <- function(maternal_age,
                                                 maternal_mental_disorder,
                                                 maternal_substance_abuse,
                                                 paternal_age,
                                                 paternal_mental_disorder,
                                                 paternal_substance_abuse,
                                                 paternal_valporate_use,
                                                 child_sex,
                                                 beta_0,
                                                 beta_maternal_age,
                                                 beta_maternal_mental_disorder,
                                                 beta_maternal_substance_abuse,
                                                 beta_paternal_age,
                                                 beta_paternal_mental_disorder,
                                                 beta_paternal_substance_abuse,
                                                 beta_paternal_valporate_use,
                                                 beta_child_sex) {
  logit_p <- beta_0 + beta_maternal_age * maternal_age +
    beta_maternal_mental_disorder * maternal_mental_disorder +
    beta_maternal_substance_abuse * maternal_substance_abuse +
    beta_paternal_age * paternal_age +
    beta_paternal_mental_disorder * paternal_mental_disorder +
    beta_paternal_substance_abuse * paternal_substance_abuse +
    beta_paternal_valporate_use * paternal_valporate_use +
    beta_child_sex * child_sex
  probability <- 1 / (1 + exp(-logit_p))
  outcome <- rbinom(1, size = 1, prob = probability)
  return(outcome)
}

child_ndd_node <- Node$new(name = "NDD presence", func =
                      simulate_outcome_logistic_regression,
                      args = list(maternal_age_node,
                                  maternal_mental_disorder_node,
                                  maternal_substance_abuse_node,
                                  paternal_age_node,
                                  paternal_mental_disorder_node,
                                  paternal_substance_abuse_node,
                                  paternal_valporate_use_node,
                                  child_sex_node,
                                  -0.5, 0.001, 0.2, 0.1, 0.001, 0.15, 0.08, 0.02, 0.05))

epi_graph <- Graph$new(list(maternal_age_node,
                            maternal_mental_disorder_node,
                            maternal_substance_abuse_node,
                            paternal_age_node,
                            paternal_mental_disorder_node,
                            paternal_substance_abuse_node,
                            paternal_valporate_use_node,
                            child_sex_node,
                            child_ndd_node), name = "EpiDAG")

output <- epi_graph$simulate(num_samples = 1000, save = TRUE)
epi_graph$draw()
