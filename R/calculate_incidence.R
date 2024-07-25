calculate_incidence <- function(output) {

  incidence_I_sub <- numeric(nrow(output))
  incidence_I_clin <- numeric(nrow(output))
  
  for (i in 2:nrow(output)) {

    incidence_I_sub[i] <- output[i, "I_sub"] - output[i-1, "I_sub"]
    incidence_I_clin[i] <- output[i, "I_clin"] - output[i-1, "I_clin"]
  }
  
  list(inc_sub = incidence_I_sub,
       inc_clin = incidence_I_clin)
}
