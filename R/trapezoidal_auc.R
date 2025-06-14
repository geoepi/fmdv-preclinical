trapezoidal_auc <- function(time_points, virus_quantities, time_unit = "hours") {
  
  # time_points to hours if days
  if (time_unit == "days") {
    time_points <- time_points * 24
  }
  
  # vector to store total virus shed for each 24-hour period
  total_virus_shed_per_period <- numeric()
  
  # loop through
  for (i in seq(1, length(time_points) - 1, by = 2)) {
    
    current_time_points <- time_points[i:(i+1)]
    current_virus_quantities <- virus_quantities[i:(i+1)]
    
    total_virus_shed <- sum((current_virus_quantities[-1] + current_virus_quantities[-length(current_virus_quantities)]) / 2 * diff(current_time_points))
    
    total_virus_shed_per_period <- c(total_virus_shed_per_period, total_virus_shed)
  }
  
  return(total_virus_shed_per_period)
}
