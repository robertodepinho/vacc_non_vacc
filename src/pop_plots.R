
#PLOT VACC X N VACC
plot_vacc_non_vacc <- function(data){
  data %>%
  ggplot(aes(x=x_neat, y=y_neat))+
    
    geom_text(aes(label = label),color = person_color, family = "fontawesome-webfont", 
              size = point_size, show.legend = FALSE)+
    geom_point(shape = 1, aes(alpha = vacc), size = shield_size, color = shield_color) +
    
    scale_alpha_manual(name = element_blank(),
                       values = c(1,0), 
                       breaks = c(T,F),
                       labels = c("Vacina",element_blank())) +
    guides(alpha  = guide_legend(override.aes = list(color = "black")),   
           nrow=2, byrow=TRUE) +
    
    theme_void() + theme(legend.position="bottom")
  
}

#PLOT VACC X N VACC X COVID
plot_vacc_non_vacc_covid  <- function(data){
  data %>%
    ggplot(aes(x=x_neat, y=y_neat))+
    
    
    geom_point(shape = 1, alpha = 0, size = 1, aes(color = infected)) +
    geom_text(aes(color = infected,label = label),family = "fontawesome-webfont",
              size = point_size, show.legend = FALSE)+
    
    geom_point(shape = 1, aes(alpha = vacc), size = shield_size, color = shield_color) +
    
    scale_color_manual(name = "Infecção",
                       values = c(covid_color,person_color),
                       breaks = c(T, F),
                       labels = c("Covid-19","Não Infectada")) +
    guides(color  = guide_legend(override.aes = list(shape = shape_guide,
                                                     fill=c(covid_color,person_color),
                                                     alpha = 1, size = 5))) +
    
    scale_alpha_manual(name = element_blank(),
                       values = c(1,0),
                       breaks = c(T,F),
                       labels = c("Vacina",element_blank())) +
    guides(alpha  = guide_legend(override.aes = list(color = "black")),
           nrow=2, byrow=TRUE) +
    
    theme_void() + theme(legend.position="bottom")
}


deprecated_plot_covid_only <- function(data){
  vacc_infected_n <- data %>% group_by(vacc) %>%
    summarise(n = n())
  
  #heights X widths
  #choose column width so that the higher group reaches the top 
  points_dist = 100/sqrt(population_size) #target mean distance between points
  number_of_cols = max(3,max(vacc_infected_n$n / sqrt(nrow(data)))) # in columns
  col_width_dist = number_of_cols * points_dist
  points_per_col = round(sqrt(population_size))
  
  return(data %>%
    arrange(desc(vacc), desc(outcome)) %>%
    group_by(vacc) %>%
    mutate( x_bar = runif(n()) * col_width_dist  - vacc*col_width_dist,
            y_bar = runif(n()) * n()/number_of_cols * points_dist) %>%
    mutate( x_bar_neat = points_dist * 
              (floor((row_number()-1) / (n()%/%number_of_cols+1)))  - vacc*col_width_dist,
            y_bar_neat = points_dist * ((row_number()-1) %% (n()%/%number_of_cols+1))) %>%
    ungroup() %>%
    ggplot(aes(x=x_bar_neat, y=y_bar_neat)) +  
    geom_point(shape = 1, alpha = 0, size = 1, aes(color = infected)) + #dummy point for legend
    geom_text(aes(color = infected,label = label),family = "fontawesome-webfont", 
              size = point_size, show.legend = FALSE) +
    
    geom_point(shape = 1, aes(alpha = vacc), size = shield_size, color = shield_color) +
    
    scale_color_manual(name = "Infecção",
                       values = c(covid_color,person_color), 
                       breaks = c(T, F),
                       labels = c("Covid-19","Não Infectada")) +
    guides(color  = guide_legend(override.aes = list(shape = shape_guide,
                                                     fill=covid_color, 
                                                     alpha = 1, size = 5))) + 
    
    scale_alpha_manual(name = element_blank(),
                       values = c(1,0), 
                       breaks = c(T,F),
                       labels = c("Vacina",element_blank())) +
    guides(alpha  = guide_legend(override.aes = list(color = "black")),   
           nrow=2, byrow=TRUE) +
    
    theme_void() + theme(legend.position="bottom"))

}