

#coin toss function with given probability.
coin_toss <- function(size, prob) {
  return(sample(x = c(TRUE,FALSE),
                replace = TRUE,
                size = size,
                prob = prob))
}

#Vaccine Effectiveness https://www.cdc.gov/csels/dsepd/ss1978/lesson3/section6.html 
ve <- function(n, n_infected, tot_infected, population_size) {
  return(1- (n_infected/n) / ((tot_infected-n_infected)/(population_size-n)))
}


#create initial population
get_population <- function(population_size) {
  #return existing population if the same size as requested
  if(exists("pop_global")){
    if(nrow(pop_global) == population_size)
      return(pop_global)
  }
  model = get_infected_2_death_model()
  pop_local <- 
    tibble(x = runif(population_size)*99+1, 
           y = runif(population_size)*99+1) %>%
    
    mutate(label = ifelse(coin_toss(size = population_size, 
                                    prob = c(0.5,0.5)),
                          fontawesome('fa-male'),
                          fontawesome('fa-female'))) %>%
    mutate(infected_if_vacc = coin_toss(size = population_size,
                                        prob = c(model[["infected_if_vacc_prob"]],
                                                 1-model[["infected_if_vacc_prob"]]))) %>%
    mutate(hosp_if_vacc_inf = coin_toss(size = population_size,
                                        prob = c(model[["hosp_if_vacc_prob"]],
                                                 1-model[["hosp_if_vacc_prob"]]))) %>%
    mutate(icu_if_vacc_inf = coin_toss(size = population_size,
                                       prob = c(model[["icu_if_vacc_prob"]],
                                                1-model[["icu_if_vacc_prob"]]))) %>%
    mutate(death_if_vacc_inf = coin_toss(size = population_size,
                                         prob = c(model[["death_if_vacc_prob"]],
                                                  1-model[["death_if_vacc_prob"]]))) %>%
    mutate(hosp_if_n_vacc_inf = coin_toss(size = population_size,
                                          prob = c(model[["hosp_if_infected_prob"]],
                                                   1-model[["hosp_if_infected_prob"]]))) %>%
    mutate(icu_if_n_vacc_inf = coin_toss(size = population_size,
                                         prob = c(model[["icu_if_infected_prob"]],
                                                  1-model[["icu_if_infected_prob"]]))) %>%
    mutate(death_if_n_vacc_inf = coin_toss(size = population_size,
                                           prob = c(model[["death_if_infected_prob"]],
                                                    1-model[["death_if_infected_prob"]])))
  
  
  pop_global <<- pop_local
  return(pop_local)
}


get_infected_2_death_model <- function() {
  #Vaxzevria/Fiocruz VE Fully vacc Table 1
  infected_if_vacc_prob = 1-0.729 #Probability of being vaccinated and still get covid if exposed enough to get covid unvaccinated.
  
  #Probability of being hospitalized in ICU/Death  if infected enough to get covid unvaccinated.
  #From Table 1
  #hosp_if_infected_prob =  (hosp_events / hosp_person-days) / (infected_events / infected_person-days)
  hosp_if_infected_prob =  (22449 / 607756996) / (130302 / 607095423)
  
  icu_if_infected_prob =  (7558 / 607855737) / (130302 / 607095423)
  
  death_if_infected_prob =  (7037 / 607859573) / (130302 / 607095423)
  
  #From Table 1
  #prob = 1 - VE 
  # hosp_if_infected_prob * (1-VE) / infected_if_vacc_prob
  hosp_if_vacc_prob = hosp_if_infected_prob * (1-0.88) / infected_if_vacc_prob
  icu_if_vacc_prob = icu_if_infected_prob * (1-0.891) / infected_if_vacc_prob
  death_if_vacc_prob = death_if_infected_prob * (1-0.902) / infected_if_vacc_prob
  
  return(list(infected_if_vacc_prob= infected_if_vacc_prob, 
              hosp_if_infected_prob = hosp_if_infected_prob,
              icu_if_infected_prob = icu_if_infected_prob,
              death_if_infected_prob = death_if_infected_prob,
              hosp_if_vacc_prob = hosp_if_vacc_prob,
              icu_if_vacc_prob = icu_if_vacc_prob,
              death_if_vacc_prob = death_if_vacc_prob) )
  
}


population_by_vacc_per <- function(vacc_per) {
  return(get_population(population_size = population_size) %>%
           add_outcomes(vacc_per) %>%
           add_neat_x_y()
  )
}


#add outcomes
add_outcomes <- function(population, vacc_per){
  model = get_infected_2_death_model()
  population_size = nrow(population)
  return(population %>% 
           mutate( vacc = x <= vacc_per,                  #vaccinated if x <= percentage of population vaccinated
                   infected = (!vacc | infected_if_vacc), #infected if not vaccinated or based on coin toss on VE. 
                   
                   #hosp/icu/death if infected & based on vacc, non vacc respective probability
                   hosp  = infected & ifelse(vacc, 
                                             hosp_if_vacc_inf,
                                             hosp_if_n_vacc_inf),
                   icu   = infected & ifelse(vacc, 
                                             icu_if_vacc_inf,
                                             icu_if_n_vacc_inf),
                   death = infected & ifelse(vacc, 
                                             death_if_vacc_inf,
                                             death_if_n_vacc_inf)) %>% 
           mutate( outcome = case_when(death==T ~ "death",
                                       hosp==T ~ "hosp",
                                       infected==T ~ "infected",
                                       TRUE ~"none") ) 
  )
}

#add neat coords
add_neat_x_y <- function(population){
  population_size = nrow(population)
  points_dist = 100/sqrt(population_size) 
  points_per_col = round(sqrt(population_size))  
  return(population %>%
           arrange(desc(vacc), desc(outcome)) %>%
           mutate(rn = row_number(), 
                  x_neat = points_dist * (floor((row_number()-1) / points_per_col)),
                  y_neat = points_dist * ((row_number()-1) %% points_per_col))
  )
}

