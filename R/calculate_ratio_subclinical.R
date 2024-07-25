calculate_ratio_subclinical <- function(df) {

  interv_inc <- df %>%
    filter(compartment %in% c("clin_inc", "sub_inc"))

  interv_inc[, 3:7] <- round(interv_inc[, 3:7], 0)
  
  ratio_sub <- interv_inc %>%
    group_by(compartment) %>%
    summarise(total = sum(Q_0.5))
  
  return(ratio_sub)
}
