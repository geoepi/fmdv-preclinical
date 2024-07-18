create_euth_column <- function(df) {
  df <- df %>%
    group_by(animal) %>%
    mutate(euth = ifelse(date == max(date), 1, 0)) %>%
    ungroup() 
  
  return(df)
}
