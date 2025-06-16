#!/usr/bin/env Rscript

# Simulation loop
# ------------------------------------------
# This script runs multiple iterations of a between-farm disease spread simulation
# using configurations from a YAML file and outputs summary statistics for each run.
# It evaluates outcomes under both clinical-only and preclinical transmission scenarios.
# Results are saved in chunked batches to CSV files for downstream analysis.
#
# Dependencies: challengeABM, igraph, data.table, dplyr, purrr, yaml, here, parallel
# Repository: https://github.com/geoepi/challengeABM

library(here)        # directory management
library(data.table)  # high-performance data frames
library(yaml)        # read configuration from .yaml
library(dplyr)       # data manipulation
library(purrr)       # functional iteration
library(igraph)      # farm network construction
library(parallel)    # parallel iteration
library(challengeABM) # custom simulation code

# set number of threads
data.table::setDTthreads(Sys.getenv("OMP_NUM_THREADS"))

# configuration file
default_config <- yaml::read_yaml("base_config_fmdv.yaml")

# run parameters
num_iterations <- 1000 # number of iterations (each iteration includes preclin and clinical runs)
write_interval <- 50 # how often to write .csv outputs
n_cores <- as.integer(Sys.getenv("OMP_NUM_THREADS")) # see accompanying .sh

# simulation wrapper for a single iteration
simulate_one <- function(iter) {
  seed      <- sample.int(.Machine$integer.max, 1) # random seed
  n_farms   <- sample(20:50, 1) # range for number of farms
  det_delay <- sample(48:168, 1) # time to quarantine (i.e, post clinical detection lockdown)

  config <- default_config # update config parameters, see, /config
  config$seed                  <- seed
  config$num_farms             <- n_farms
  config$detection_delay_hours <- det_delay

  output <- list()

  for (preclin in c(FALSE, TRUE)) { # both scenarios with identical settings
    config$preclin_infect <- preclin
    set.seed(seed)

    net <- generate_farm_network(config) # simulation function to create random graph
    res <- simulate_net_model(net, config) # run simulation using graph and params

    # extract results
    farm_states <- res$farm_status$herd_states
    move_df     <- bind_rows(res$movement_log)

    # SUMMARIZE RESULTS
    ## number of secondary infected farms (R_farm)
    infected_flags <- map_lgl(farm_states, ~ any(.x$infection_status %in% c("infected", "recovered")))
    srcs <- names(infected_flags)[infected_flags]
    sec_counts <- sapply(srcs, function(s) {
      dests <- unique(move_df$to[move_df$from == s])
      sum(infected_flags[dests], na.rm = TRUE)
    })
    R_farm <- if (length(sec_counts) > 0) mean(sec_counts) else 0
    outbreak_farms <- sum(infected_flags)

    # first clinical infection time
    clinical_times <- map_dbl(farm_states, function(df) {
      times <- df$time[df$state == "clinical" & df$time > 0 & df$is_donor == FALSE]
      if (length(times)) min(times) else NA_real_
    })
    first_inf_time <- min(clinical_times, na.rm = TRUE)

    # infected animals at first clinical detection
    total_at_first <- sum(
      map_int(farm_states, ~ sum(
        .x$time == first_inf_time &
          .x$infection_status == "infected" &
          .x$is_donor == FALSE
      ))
    )

    # number of non-donor animals infected
    total_animals <- sum(
      map_int(res$farm_status$herd_agents, ~ sum(
        .x$infection_status %in% c("infected", "recovered") &
          .x$is_donor == FALSE
      ))
    )

    record <- tibble(
      iteration                = iter,
      seed                     = seed,
      num_farms                = n_farms,
      detection_delay_hours    = det_delay,
      preclin_infect           = preclin,
      R_farm                   = R_farm,
      outbreak_farms           = outbreak_farms,
      first_infection_time     = first_inf_time,
      total_infected_at_first  = total_at_first,
      total_infected_animals   = total_animals
    )

    output[[length(output) + 1]] <- record

    rm(net, res, farm_states, move_df)
    gc()
  }

  data.table::rbindlist(output)
}

# chunked execution with periodic write
iters <- seq_len(num_iterations)
chunked_iters <- split(iters, ceiling(iters / write_interval))

for (chunk in chunked_iters) {
  message(sprintf("[%s] Running iterations %d to %d", Sys.time(), min(chunk), max(chunk)))
  results <- mclapply(chunk, simulate_one, mc.cores = n_cores)
  df_out <- data.table::rbindlist(results)
  fname  <- here("sim_out", sprintf("batch_results_iter%03d.csv", max(chunk)))
  data.table::fwrite(df_out, fname)
}
