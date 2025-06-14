simpsons_auc <- function(time_points, virus_quantities, time_unit = "days") {
  
  if (time_unit == "days") {
    time_points <- time_points * 24
  }
  
  simpsons <- Bolstad2::sintegral(time_points, 
                                  virus_quantities, 
                                  n.pts = max(time_points)/24)
  
  peak_index <- which.max(virus_quantities)
  time_of_peak <- time_points[peak_index]
  virus_peak_value <- virus_quantities[peak_index]
  
  return(simpsons)
}

