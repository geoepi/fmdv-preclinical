library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

left_skewed_gamma <- function(desired_mean, shape, size = 1) {
  
  scale <- desired_mean / shape
  
  gamma_sample <- rgamma(size, shape, scale)
  
  left_skewed_sample <- (desired_mean) - gamma_sample
  
  return(left_skewed_sample)
}


# initialize agents
initialize_abm_agents <- function(num_infected, num_non_infected_per_room, num_rooms,
                                  initial_infected_virus_nasal, initial_infected_virus_serum,
                                  nasal_threshold_mean, serum_threshold_mean, nasal_shape,
                                  serum_threshold_sd, infect_threshold_mean, infect_threshold_sd) {
  #random draws
  nasal_threshold = left_skewed_gamma(nasal_threshold_mean, nasal_shape)
  serum_threshold = rnorm(num_infected, mean = serum_threshold_mean, sd = serum_threshold_sd)
  infect_threshold = rnorm(num_infected, mean = infect_threshold_mean, sd = infect_threshold_sd)
  
  # initial infected agents
  infected_agents <- data.frame(
    id = 1:num_infected,
    infection_status = "infected",
    infect_agent = TRUE,
    is_donor = TRUE,  
    room = 1,
    time_in_room = 0,
    virus_nasal = initial_infected_virus_nasal,
    virus_serum = initial_infected_virus_serum,
    score_t = 0, 
    nasal_threshold = nasal_threshold,  
    serum_threshold = serum_threshold,
    infect_threshold = infect_threshold,  
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point = NA,
    inflection_point_absolute = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = 0  
  )
  
  num_non_infected_agents <- num_non_infected_per_room * (num_rooms - 1)
  non_inf_nasal_threshold = left_skewed_gamma(nasal_threshold_mean, nasal_shape)  
  non_inf_serum_threshold = rnorm(num_non_infected_agents, mean = serum_threshold_mean, sd = serum_threshold_sd)
  non_inf_infect_threshold = rnorm(num_non_infected_agents, mean = infect_threshold_mean, sd = infect_threshold_sd)
  
  # non-infected agents for all rooms
  non_infected_agents <- data.frame(
    id = (num_infected + 1):(num_infected + num_non_infected_agents),
    infection_status = "non_infected",
    infect_agent = FALSE,
    is_donor = FALSE, 
    room = rep(2:num_rooms, each = num_non_infected_per_room),
    time_in_room = 0,
    virus_nasal = 0,
    virus_serum = 0,
    score_t = 0,  
    nasal_threshold = non_inf_nasal_threshold,  
    serum_threshold = non_inf_serum_threshold,
    infect_threshold = non_inf_infect_threshold,
    dose = NA,
    infectious_t = NA,
    growth_rate_nasal = NA,
    growth_rate_serum = NA,
    clearance_rate = NA,
    stochastic_noise = NA,
    exponential_factor = NA,
    inflection_point = NA,
    inflection_point_absolute = NA,
    growth_cease = NA,
    nasal_ccap = NA,
    serum_ccap = NA,
    infection_time = NA  
  )
  
  # join
  agents <- rbind(infected_agents, non_infected_agents)
  return(agents)
}

# fFunction to infect agent
infect_agent <- function(agent_id, agents, current_time,
                         initial_infection = FALSE, k_dose,
                         initial_virus_nasal, initial_virus_serum,
                         delta_t, dose_shape1, dose_shape2, 
                         growth_rate_nasal_mean, growth_rate_nasal_sd,
                         growth_rate_serum_mean, growth_rate_serum_sd,
                         clearance_rate_mean, clearance_rate_sd,
                         stochastic_noise_mean, stochastic_noise_sd,
                         exponential_factor_mean, exponential_factor_sd,
                         inflection_point_mean, inflection_point_sd,
                         growth_cease_mean, growth_cease_sd,
                         nasal_ccap_mean, nasal_ccap_sd,
                         serum_ccap_mean, serum_ccap_sd) {
  
  # if agent already infected return (if not the initial infection) return
  if (agents$infect_agent[agent_id] && !initial_infection) {
    return(agents)
  }
  
  # infect the agent
  agents$infect_agent[agent_id] <- TRUE
  agents$infection_status[agent_id] <- "infected"
  
  # infection_time
  agents$infection_time[agent_id] <- current_time
  
  # initial virus concentrations
  if (initial_infection) {
    # donors: set initial virus levels based on some default values
    agents$virus_nasal[agent_id] <- rnorm(1, mean = initial_virus_nasal, sd = 0.1)
    agents$virus_serum[agent_id] <- rnorm(1, mean = initial_virus_serum, sd = 0.1)
    
    # Aassign individual-specific parameters for virus growth
    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24
  } else if (!is.na(agents$dose[agent_id])) {
    # non-donors: sample virus based on dose received
    dose_eff <- rbeta(1, shape1 = dose_shape1, shape2 = dose_shape2)
    agents$virus_nasal[agent_id] <- dose_eff * agents$dose[agent_id]
    agents$virus_serum[agent_id] <- agents$virus_nasal[agent_id] #* 0.1  # Example ratio
    
    # adjust growth rates based on dose_eff
    base_growth_rate_nasal <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    base_growth_rate_serum <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24
    
    # scaling factor 'k' to control dose_eff
    k_dose <- k_dose  
    
    # Adjust growth rates
    agents$growth_rate_nasal[agent_id] <- base_growth_rate_nasal * (1 + k_dose * dose_eff)
    agents$growth_rate_serum[agent_id] <- base_growth_rate_serum * (1 + k_dose * dose_eff)
  } else {
    # default non-infected agents receiving no dose
    agents$virus_nasal[agent_id] <- 0  
    agents$virus_serum[agent_id] <- 0
    
    # base growth rates without dose adjustment
    agents$growth_rate_nasal[agent_id] <- rnorm(1, mean = growth_rate_nasal_mean, sd = growth_rate_nasal_sd) / 24
    agents$growth_rate_serum[agent_id] <- rnorm(1, mean = growth_rate_serum_mean, sd = growth_rate_serum_sd) / 24
  }
  
  # individual parameters for virus growth 
  agents$clearance_rate[agent_id] <- rnorm(1, mean = clearance_rate_mean, sd = clearance_rate_sd) / 24
  agents$stochastic_noise[agent_id] <- rnorm(1, mean = stochastic_noise_mean, sd = stochastic_noise_sd)
  agents$exponential_factor[agent_id] <- rnorm(1, mean = exponential_factor_mean, sd = exponential_factor_sd)
  
  # sample inflection point relative to infection time, in hours
  sampled_inflection_point_hours <- max(0, rnorm(1, mean = inflection_point_mean, sd = inflection_point_sd)) * 24
  agents$inflection_point[agent_id] <- sampled_inflection_point_hours  # Relative inflection point in hours
  agents$inflection_point_absolute[agent_id] <- current_time + sampled_inflection_point_hours  # Absolute time
  
  sampled_growth_cease <- rnorm(1, mean = growth_cease_mean, sd = growth_cease_sd)
  agents$growth_cease[agent_id] <- current_time + sampled_growth_cease
  
  # virus caps (carrying capacity)
  agents$nasal_ccap[agent_id] <- rnorm(1, mean = nasal_ccap_mean, sd = nasal_ccap_sd)
  agents$serum_ccap[agent_id] <- rnorm(1, mean = serum_ccap_mean, sd = serum_ccap_sd)
  
  return(agents)
}



# preps data for plotting, hold place in line with NA
process_final_results <- function(agents) {
  for (agent_id in 1:nrow(agents)) {
    if (!is.na(agents$infectious_t[agent_id]) && !is.na(agents$score_t[agent_id])) {
      if (agents$score_t[agent_id] == 0) {
        agents$infectious_t[agent_id] <- NA
      } else if (agents$infectious_t[agent_id] > agents$score_t[agent_id]) {
        agents$infectious_t[agent_id] <- agents$score_t[agent_id]
      }
    }
  }
  
  return(agents)
}



# update virus dynamics hourly
update_agent_virus_dynamics <- function(agent, current_time, delta_t) {
  
  animal_inflection_point <- agent$inflection_point_absolute  # Already in absolute time
  
  # dynamic growth rate: ccease growth after a certain time
  growth_cease <- agent$growth_cease
  
  if (current_time > growth_cease) {
    effective_growth_rate_nasal <- 0
    effective_growth_rate_serum <- 0
  } else {
    # reduced growth after the inflection point
    if (current_time <= animal_inflection_point) {
      effective_growth_rate_nasal <- agent$growth_rate_nasal  # Growth phase
      effective_growth_rate_serum <- agent$growth_rate_serum
    } else {
      effective_growth_rate_nasal <- agent$growth_rate_nasal * exp(-(current_time - animal_inflection_point) / (24 * agent$exponential_factor))
      effective_growth_rate_serum <- agent$growth_rate_serum * exp(-(current_time - animal_inflection_point) / (24 * agent$exponential_factor))
    }
  }
  
  # clearance continues
  dynamic_clearance_nasal <- agent$clearance_rate
  dynamic_clearance_serum <- agent$clearance_rate
  
  # stochastic noise
  stochastic_value_nasal <- rnorm(1, mean = 0, sd = agent$stochastic_noise)
  stochastic_value_serum <- rnorm(1, mean = 0, sd = agent$stochastic_noise)
  
  # update virus_nasal
  virus_nasal <- max(0,
                     agent$virus_nasal +
                       effective_growth_rate_nasal * agent$virus_nasal * (1 - agent$virus_nasal / agent$nasal_ccap) * delta_t -
                       dynamic_clearance_nasal * agent$virus_nasal * delta_t +
                       stochastic_value_nasal)
  
  # update virus_serum
  virus_serum <- max(0,
                     agent$virus_serum +
                       effective_growth_rate_serum * agent$virus_serum * (1 - agent$virus_serum / agent$serum_ccap) * delta_t -
                       dynamic_clearance_serum * agent$virus_serum * delta_t +
                       stochastic_value_serum)
  
  agent$virus_nasal <- virus_nasal
  agent$virus_serum <- virus_serum
  
  return(agent)
}


# check for transmission in the current room
check_and_transmit_infection <- function(agents, room, current_time,k_dose,
                                         initial_virus_nasal, initial_virus_serum,
                                         delta_t, dose_shape1, dose_shape2,
                                         growth_rate_nasal_mean, growth_rate_nasal_sd,
                                         growth_rate_serum_mean, growth_rate_serum_sd,
                                         clearance_rate_mean, clearance_rate_sd,
                                         stochastic_noise_mean, stochastic_noise_sd,
                                         exponential_factor_mean, exponential_factor_sd,
                                         inflection_point_mean, inflection_point_sd,
                                         growth_cease_mean, growth_cease_sd,
                                         nasal_ccap_mean, nasal_ccap_sd,
                                         serum_ccap_mean, serum_ccap_sd) {
  # agents in the current room
  room_agents <- agents[agents$room == room, ]
  
  # Identify transmitting agents 
  transmitting_agents <- room_agents[room_agents$infect_agent & room_agents$virus_nasal >= room_agents$infect_threshold, ]
  
  if (nrow(transmitting_agents) > 0) {
    # non-infected agents in the room
    non_infected_agents <- room_agents[!room_agents$infect_agent, ]
    
    for (non_inf_agent_id in non_infected_agents$id) {
      non_inf_agent <- agents[agents$id == non_inf_agent_id, ]
      # for each transmitting agent
      for (transmitting_agent_id in transmitting_agents$id) {
        transmitting_agent <- agents[agents$id == transmitting_agent_id, ]
        # check if transmitting agent's virus_nasal >= infect_threshold of non-infected agent
        if (transmitting_agent$virus_nasal >= non_inf_agent$infect_threshold) {
          # update the dose column 
          agents$dose[agents$id == non_inf_agent_id] <- transmitting_agent$virus_nasal
          
          # infect the non-infected agent
          agents <- infect_agent(
            agent_id = non_inf_agent_id,
            agents = agents,
            current_time = current_time,
            initial_infection = FALSE,
            initial_virus_nasal = NULL,  # Not used here
            initial_virus_serum = NULL,  # Not used here
            delta_t = delta_t,
            dose_shape1 = dose_shape1,
            dose_shape2 = dose_shape2,
            k_dose = k_dose,
            growth_rate_nasal_mean = growth_rate_nasal_mean,
            growth_rate_nasal_sd = growth_rate_nasal_sd,
            growth_rate_serum_mean = growth_rate_serum_mean,
            growth_rate_serum_sd = growth_rate_serum_sd,
            clearance_rate_mean = clearance_rate_mean,
            clearance_rate_sd = clearance_rate_sd,
            stochastic_noise_mean = stochastic_noise_mean,
            stochastic_noise_sd = stochastic_noise_sd,
            exponential_factor_mean = exponential_factor_mean,
            exponential_factor_sd = exponential_factor_sd,
            inflection_point_mean = inflection_point_mean,
            inflection_point_sd = inflection_point_sd,
            growth_cease_mean = growth_cease_mean,
            growth_cease_sd = growth_cease_sd,
            nasal_ccap_mean = nasal_ccap_mean,
            nasal_ccap_sd = nasal_ccap_sd,
            serum_ccap_mean = serum_ccap_mean,
            serum_ccap_sd = serum_ccap_sd
          )
          
          break
        }
      }
    }
  }
  
  return(agents)
}





simulate_abm_with_transmission <- function(num_infected, num_non_infected_per_room, num_hours,
                                           infect_threshold_mean, infect_threshold_sd,
                                           delta_t = 1, dose_shape1, dose_shape2,
                                           initial_virus_nasal, initial_virus_serum, k_dose,
                                           growth_rate_nasal_mean, growth_rate_nasal_sd,
                                           growth_rate_serum_mean, growth_rate_serum_sd,
                                           clearance_rate_mean, clearance_rate_sd, nasal_shape,
                                           stochastic_noise_mean, stochastic_noise_sd,
                                           exponential_factor_mean, exponential_factor_sd,
                                           inflection_point_mean = 4, inflection_point_sd = 0.5,
                                           growth_cease_mean = 240, growth_cease_sd = 10,
                                           nasal_ccap_mean = 10, nasal_ccap_sd = 0.5,
                                           serum_ccap_mean = 10, serum_ccap_sd = 0.5,
                                           num_rooms = 5, donor_move_interval = 24,
                                           nasal_threshold_mean, 
                                           serum_threshold_mean, serum_threshold_sd) {
  
  # initialize agents
  agents <- initialize_abm_agents(num_infected = num_infected,
                                  num_non_infected_per_room = num_non_infected_per_room,
                                  num_rooms = num_rooms, nasal_shape = nasal_shape, 
                                  initial_infected_virus_nasal = initial_virus_nasal,
                                  initial_infected_virus_serum = initial_virus_serum,
                                  nasal_threshold_mean = nasal_threshold_mean,
                                  serum_threshold_mean = serum_threshold_mean,
                                  serum_threshold_sd = serum_threshold_sd,
                                  infect_threshold_mean = infect_threshold_mean,
                                  infect_threshold_sd = infect_threshold_sd)
  
  # initialize final_results data
  final_results <- expand.grid(time = 0:(num_hours - 1), id = agents$id)
  final_results$infection_status <- "non_infected"
  final_results$virus_nasal <- 0
  final_results$virus_serum <- 0
  final_results$room <- NA  
  final_results$is_donor <- FALSE 
  final_results$score <- 0
  final_results$infectious_t <- NA
  
  # infect initial agents
  for (agent_id in 1:num_infected) {
    agents <- infect_agent(
      agent_id = agent_id,
      agents = agents,
      current_time = 0,
      initial_infection = TRUE,
      initial_virus_nasal = initial_virus_nasal,
      initial_virus_serum = initial_virus_serum,
      delta_t = delta_t,
      dose_shape1 = dose_shape1,
      dose_shape2 = dose_shape2,
      k_dose=k_dose,
      growth_rate_nasal_mean = growth_rate_nasal_mean,
      growth_rate_nasal_sd = growth_rate_nasal_sd,
      growth_rate_serum_mean = growth_rate_serum_mean,
      growth_rate_serum_sd = growth_rate_serum_sd,
      clearance_rate_mean = clearance_rate_mean,
      clearance_rate_sd = clearance_rate_sd,
      stochastic_noise_mean = stochastic_noise_mean,
      stochastic_noise_sd = stochastic_noise_sd,
      exponential_factor_mean = exponential_factor_mean,
      exponential_factor_sd = exponential_factor_sd,
      inflection_point_mean = inflection_point_mean,
      inflection_point_sd = inflection_point_sd,
      growth_cease_mean = growth_cease_mean,
      growth_cease_sd = growth_cease_sd,
      nasal_ccap_mean = nasal_ccap_mean,
      nasal_ccap_sd = nasal_ccap_sd,
      serum_ccap_mean = serum_ccap_mean,
      serum_ccap_sd = serum_ccap_sd
    )
  }
  
  # each hour
  for (current_time in 0:(num_hours - 1)) {
    
    agents$time_in_room <- agents$time_in_room + 1
    
    for (agent_id in which(agents$infect_agent)) {
      agent <- agents[agent_id, ]
      agent <- update_agent_virus_dynamics(agent, current_time, delta_t)
      agents[agent_id, ] <- agent
      
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$virus_nasal[idx] <- agent$virus_nasal
      final_results$virus_serum[idx] <- agent$virus_serum
      final_results$infection_status[idx] <- "infected"
      
      if (is.na(agent$infectious_t) && agent$virus_nasal >= agent$infect_threshold) {
        agent$infectious_t <- current_time  # Record the time the agent becomes infectious
        agents$infectious_t[agent_id] <- agent$infectious_t
      }
      
      idx <- final_results$id == agent_id & final_results$time == current_time
      if (!is.na(agents$infectious_t[agent_id])) {
        final_results$infectious_t[idx] <- agents$infectious_t[agent_id]
      }
      

      if (agent$score_t == 0) { 
        if (agent$virus_nasal >= agent$nasal_threshold || agent$virus_serum >= agent$serum_threshold) {
      
          agent$score_t <- current_time  
          agents$score_t[agent_id] <- agent$score_t
          
          final_results$score[final_results$id == agent_id & final_results$time >= current_time] <- 1
        }
      }
      
      if (is.na(agent$infectious_t) && agent$virus_nasal >= agent$infect_threshold) {
        agent$infectious_t <- current_time  
        agents$infectious_t[agent_id] <- agent$infectious_t
      }
    }
    
    if ((current_time + 1) %% donor_move_interval == 0 && agents$room[1] < num_rooms) {
      agents$room[agents$id <= num_infected] <- agents$room[agents$id <= num_infected] + 1
      agents$time_in_room[agents$id <= num_infected] <- 0
    }
    
    for (agent_id in agents$id) {
      idx <- final_results$id == agent_id & final_results$time == current_time
      final_results$room[idx] <- agents$room[agent_id]
      final_results$is_donor[idx] <- agents$is_donor[agent_id]
    }
    
    for (room in unique(agents$room)) {
      agents <- check_and_transmit_infection(
        agents = agents,
        room = room,
        current_time = current_time,
        initial_virus_nasal = initial_virus_nasal,
        initial_virus_serum = initial_virus_serum,
        delta_t = delta_t,
        dose_shape1 = dose_shape1,
        dose_shape2 = dose_shape2,
        k_dose = k_dose,
        growth_rate_nasal_mean = growth_rate_nasal_mean,
        growth_rate_nasal_sd = growth_rate_nasal_sd,
        growth_rate_serum_mean = growth_rate_serum_mean,
        growth_rate_serum_sd = growth_rate_serum_sd,
        clearance_rate_mean = clearance_rate_mean,
        clearance_rate_sd = clearance_rate_sd,
        stochastic_noise_mean = stochastic_noise_mean,
        stochastic_noise_sd = stochastic_noise_sd,
        exponential_factor_mean = exponential_factor_mean,
        exponential_factor_sd = exponential_factor_sd,
        inflection_point_mean = inflection_point_mean,
        inflection_point_sd = inflection_point_sd,
        growth_cease_mean = growth_cease_mean,
        growth_cease_sd = growth_cease_sd,
        nasal_ccap_mean = nasal_ccap_mean,
        nasal_ccap_sd = nasal_ccap_sd,
        serum_ccap_mean = serum_ccap_mean,
        serum_ccap_sd = serum_ccap_sd
      )
    }
  }
  
  agents <- process_final_results(agents)
  
  return(list(agents = agents, final_results = final_results))
}


num_hours <- 240  
seed_val <- round(runif(1, 100, 300),0)
set.seed(seed_val)


ui <- fluidPage(
  titlePanel("ABM Simulation: FMDV Challenge Experiment"),
  
  fluidRow(
    column(width = 12, align = "center",
           actionButton("run_btn", "Run Experiment")
    )
  ),
  
  fluidRow(
    column(width = 12, align = "center",
           textOutput("seed_value")
    )
  ),
  
  br(),
  
  plotOutput("status_plot", height = "700px"),
  
  br(),
  
  br(),
  
  plotOutput("trajectory_plot", height = "1000px")
)

server <- function(input, output) {
  
  simulation_result <- eventReactive(input$run_btn, {

    seed_val <- as.numeric(Sys.time())
    set.seed(seed_val)
    
    model_result <- simulate_abm_with_transmission(
      num_infected = 2,
      num_non_infected_per_room = 4,
      num_hours = 240,
      delta_t = 1,
      dose_shape1 = 1,
      dose_shape2 = 15,
      initial_virus_nasal = 0.001,
      initial_virus_serum = 0.001,
      k_dose = 0.2,
      nasal_threshold_mean = 7.98,
      nasal_shape = 2,
      serum_threshold_mean = 10.83,
      serum_threshold_sd = 0.903,
      infect_threshold_mean = 5.8,
      infect_threshold_sd = 0.7337373,
      growth_rate_nasal_mean = 4.1,
      growth_rate_nasal_sd = 0.1,
      growth_rate_serum_mean = 3.0,
      growth_rate_serum_sd = 0.3,
      clearance_rate_mean = 1.0,
      clearance_rate_sd = 0.1,
      stochastic_noise_mean = 0.21,
      stochastic_noise_sd = 0.05,
      exponential_factor_mean = 0.5,
      exponential_factor_sd = 0.15,
      inflection_point_mean = 4,
      inflection_point_sd = 0.5,
      growth_cease_mean = 240,
      growth_cease_sd = 10,
      nasal_ccap_mean = 10,
      nasal_ccap_sd = 0.5,
      serum_ccap_mean = 10,
      serum_ccap_sd = 0.5,
      num_rooms = 5,
      donor_move_interval = 24
    )
    
    # Return the model result and the seed value
    list(model_result = model_result, seed_val = seed_val)
  })
  
  # Display the seed value
  output$seed_value <- renderText({
    req(simulation_result())
    paste("Seed value:", simulation_result()$seed_val)
  })
  
  # Create the first plot
  output$status_plot <- renderPlot({
    # Only render the plot if the button has been pressed
    req(simulation_result())
    
    # Extract the data from the simulation result
    status_data <- simulation_result()$model_result$final_results %>%
      group_by(id) %>%
      summarize(
        infection_start = ifelse(any(infection_status == "infected"), min(time[infection_status == "infected"], na.rm = TRUE), NA),
        infectious_start = ifelse(any(!is.na(infectious_t)), min(time[!is.na(infectious_t)], na.rm = TRUE), NA),
        symptoms_start = ifelse(any(score > 0), min(time[score > 0], na.rm = TRUE), NA),
        total_time = max(time)
      ) %>%
      mutate(
        non_infected_start = 0,
        non_infected_end = ifelse(is.na(infection_start), total_time, infection_start),
        infected_non_infectious_start = infection_start,
        infected_non_infectious_end = ifelse(!is.na(infectious_start), infectious_start, total_time),
        infectious_start_time = infectious_start,
        infectious_end = ifelse(!is.na(symptoms_start), symptoms_start, total_time),
        symptomatic_start = symptoms_start,
        symptomatic_end = ifelse(!is.na(symptoms_start), total_time, NA)
      ) %>%
      pivot_longer(
        cols = starts_with(c("non_infected", "infected_non_infectious", "infectious", "symptomatic")),
        names_to = c("status", ".value"),
        names_pattern = "(.*)_(start|end)"
      ) %>%
      filter(!is.na(start) & !is.na(end)) %>%
      mutate(duration = end - start) %>%
      filter(duration > 0) %>%
      mutate(status = factor(status, levels = c("non_infected", "infected_non_infectious",
                                                "infectious", "symptomatic")))
    
    ggplot(status_data, aes(xmin = start, xmax = end, ymin = id - 0.4, 
                            ymax = id + 0.4, fill = status)) +
      geom_rect() +
      scale_fill_manual(
        values = c(
          "non_infected" = "lightblue",
          "infected_non_infectious" = "orange",
          "infectious" = "red",
          "symptomatic" = "darkred"
        ),
        labels = c(
          "Non-Infected", 
          "Infected (Non-Infectious)", 
          "Subclinical Infectious", 
          "Clinical Infectious"
        ),
        guide = guide_legend(nrow = 2)
      ) +
      labs(
        x = "Simulation Time (Hours)",
        y = "Animal",
        fill = "Status",
        title = "Animal Status"
      ) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(breaks = unique(status_data$id)) + 
      theme_minimal() +
      theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
            legend.direction = "horizontal",
            legend.position="bottom",
            strip.text = element_blank(), 
            strip.background = element_blank(),
            legend.key.size = unit(2,"line"),
            legend.key.width = unit(3,"line"),
            legend.text = element_text(size=16, face="bold"),
            legend.title = element_text(size=18, face="bold"),
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=22, face="bold"),
            axis.text.x = element_text(face="bold", size=15, vjust=0.5),
            axis.text.y = element_text(size=18, face="bold"),
            plot.title = element_text(size=22, face="bold")
      )
  })
  
  output$trajectory_plot <- renderPlot({

    req(simulation_result())
    
    model_result <- simulation_result()$model_result
    agent_trajectories <- model_result$final_results
    
    agent_trajectories$group <- cut(agent_trajectories$id,
                                    breaks = c(-Inf, 2, 6, 10, 14, 18),
                                    labels = c("Donors", "Group 1", "Group 2", "Group 3", "Group 4"),
                                    right = TRUE)
    
    agent_trajectories <- agent_trajectories %>%
      select(time, group, id, virus_nasal, virus_serum)
    
    names(agent_trajectories) <- c("Time", "Group", "Animal", "Nasal", "Serum")
    
    long_data <- agent_trajectories %>%
      pivot_longer(cols = c(Nasal, Serum), names_to = "Type", values_to = "Value") %>%
      mutate(Animal = as.factor(Animal))
    
    ggplot(long_data, aes(x = Time, y = Value, color = Animal, group = Animal)) +
      geom_line() +
      facet_wrap(~ Group + Type, ncol = 2, scales = "free_y") +
      labs(title = "Nasal and Serum Dynamics",
           x = "Time",
           y = "RNA (log10 copies/ml)") +
      scale_color_viridis_d(option = "turbo", name = "Animal", guide = guide_legend(nrow = 3)) + 
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_minimal() +
      theme(plot.margin = unit(c(1,0.75,1,0.75),"cm"),
            legend.direction = "horizontal",
            legend.position="bottom",
            strip.text = element_text(size=14, face="bold"),
            strip.background = element_blank(),
            legend.key.size = unit(2,"line"),
            legend.key.width = unit(3,"line"),
            legend.text = element_text(size=16, face="bold"),
            legend.title = element_text(size=18, face="bold"),
            axis.title.x = element_text(size=18, face="bold"),
            axis.title.y = element_text(size=22, face="bold"),
            axis.text.x = element_text(face="bold", size=15, vjust=0.5),
            axis.text.y = element_text(size=18, face="bold"),
            plot.title = element_text(size=22, face="bold")
      )
    

  })
}

# Run 
shinyApp(ui = ui, server = server)