find_closest_quant <- function(df, target_value) {
  
  # Ensure required columns are present
  required_columns <- c("quant0.025", "quant0.25", "quant0.5", "quant0.75", "quant0.975")
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(paste("Dataframe must contain the columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Initialize result list
  results <- list()
  
  # Loop through each quantile column
  for (col in required_columns) {
    df$diff <- abs(df[[col]] - target_value)
    min_diff_index <- which.min(df$diff)
    results[[col]] <- df[min_diff_index, "time_vect"]
  }
  
  return(results)
}
