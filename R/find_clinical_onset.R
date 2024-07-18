find_clinical_onset <- function(df) {
  
  df <- df[order(df$animal, df$date), ]
  
  df$score[is.na(df$score)] = 0
  
  df$Event <- 0
  df$censor_k <- 0
  
  animals_vect <- unique(df$animal)
  for (i in 1:length(animals_vect)) {
    
    animal_rows <- df %>%
      filter(animal == animals_vect[i])
    
    if(any(animal_rows$censor_status == 0)) {
      
      latest_censor_date <- max(animal_rows$date[animal_rows$censor_status == 0], na.rm = TRUE)
      
      df$censor_k[df$animal == animals_vect[i] & df$date == latest_censor_date] <- 1
    }
    
    if(unique(animal_rows$censor_status) == 0) next

      exceed_date <- min(animal_rows$date[animal_rows$score > 0], na.rm = TRUE)
    
      df$Event[df$animal == animals_vect[i] & df$date == exceed_date] <- 1
      
      df$Event[df$animal == animals_vect[i] & df$date > exceed_date] <- 3
  }
  
  return(df)
}
