#####scrapbook
#<!-- `r textOutput("population_size_text")` -->
# geom_text(color = "black",family = "fontawesome-webfont", size = 5, 
#           label = fontawesome('fa-shield')  )+
# 
# scale_color_manual(values = c(covid_color,border_color), 
#                    breaks = c(T,F),
#                    labels = c("Infectada", "Não infectada"))+
# scale_color_manual(values = c(person_color,covid_color), 
#                    breaks = c(T,F),
#                    labels = c("Vacinada", "Não vacinada"))+
#guides(color = "none") + 
# scale_shape_manual(name = "Vacina",
#                    values = c(shape_vacc,shape_n_vacc), 
#                    breaks = c(T,F),
#                    labels = c("Vacinada","Não vacinada")) +
# guides(shape = guide_legend(override.aes = list(fill = person_color, 
#                                                   color = border_color),
#                               nrow=2,byrow=TRUE)) +
#   
#shape_vacc = 23
#shape_n_vacc = 25
#points_dist = 100/sqrt(population_size) #target mean distance between points

#pop_infected <- population_outcome() %>%
#  filter(infected)
#vacc_infected_n <- pop_infected %>% group_by(vacc) %>%
#  summarise(n = n())
#max_xy_vacc = sqrt(vacc_infected_n$n) * points_dist
#print(max_xy_vacc)

# chart_completo <- function(){
#   population_outcome() %>%
#     ggplot(aes(x=x_neat, y=y_neat))+
#     geom_point(shape = 1, alpha = 0, size = 1, aes(color = outcome)) +
#     geom_text(aes(color = outcome,label = label),family = "fontawesome-webfont", 
#               size = point_size, show.legend = FALSE)+
#     
#     geom_point(shape = 1, aes(alpha = vacc), size = shield_size, color = "grey") +
#     
#     scale_color_manual(name = "Resultado",
#                        values = c(death_color, hosp_color,covid_color,person_color), 
#                        breaks = c("death", "hosp","infected", "none"),
#                        labels = c("Morte","Hospitalização","Covid-19","Não Infectada")) +
#     guides(color  = guide_legend(override.aes = list(shape = shape_guide,
#                                                      fill=c(death_color, hosp_color,
#                                                             covid_color,person_color), 
#                                                      alpha = 1, size = 5),
#                                  nrow=2,byrow=TRUE)) +
#     
#     scale_alpha_manual(name = element_blank(),
#                        values = c(1,0), 
#                        breaks = c(T,F),
#                        labels = c("Vacina",element_blank())) +
#     guides(alpha  = guide_legend(override.aes = list(color = "black")),   
#            nrow=2, byrow=TRUE) +
#     
#     theme_void() + theme(legend.position="bottom")
#   
# }



#XX - PLOT VACC X N VACC X COVID WAFFLE
deprecated_plot_vacc_non_vacc_covid_sort  <- function(data){
  return(data %>%
           
           mutate(label_vacc = ifelse(vacc, emoji('white_circle'),"")) %>%
           mutate(vacc = factor(vacc, 
                                levels = c(T,F), 
                                labels = c("Vacinada","Não vacinada"))) %>%
           mutate(infected = factor(infected, 
                                    levels = c(T,F), ordered = T)) %>%
           select(vacc, label, label_vacc, infected) %>%
           group_by(vacc, infected, label, label_vacc) %>%
           summarise(n = n()) %>%
           
           
           ggplot(aes( values = n))  +
           geom_pictogram(aes(label = label, color = infected),
                          n_rows = 20, 
                          size = 4, flip = TRUE,
                          family = "fontawesome-webfont", show.legend = F) +
           geom_pictogram(aes(label = " ", color = infected),
                          n_rows = 20, size = 4, flip = TRUE,
                          family = "sans") + #dummy empty pictogram for legend
           geom_pictogram(color = shield_color, fill = "red",
                          aes(label = label_vacc ),
                          n_rows = 20, size = 7, flip = TRUE,
                          family = "EmojiOne", show.legend = F) + 
           
           coord_equal() +
           theme_enhance_waffle() +
           facet_wrap(~vacc) +
           scale_color_manual(name = "Infecção",
                              values = c(person_color, covid_color, "white"),
                              breaks = c(F, T, "white"),
                              labels = c("Não Infectada", "Covid-19","")) +
           guides(color  = guide_legend( override.aes = list(size = 10, label = rep("■",1))))+
           theme_void() + theme(legend.key = element_blank(), legend.position="bottom")
  )}


#XX - PLOT VACC X N VACC X HOSP x DEATH SORT
deprecated_plot_hosp_death_sort <- function(data){
  (data %>%
     mutate(label_vacc = ifelse(vacc, emoji('white_circle'),"")) %>%
     mutate(vacc = factor(vacc, 
                          levels = c(T,F), 
                          labels = c("Vacinada","Não vacinada"))) %>%
     select(vacc, label, label_vacc, outcome) %>%
     group_by(vacc, outcome, label, label_vacc) %>%
     summarise(n = n()) %>%
     ggplot(aes( values = n))  +
     geom_pictogram(aes(label = label, color = outcome),
                    n_rows = 20, size = 4, flip = TRUE,
                    family = "fontawesome-webfont", show.legend = F) +
     geom_pictogram(aes(label = " ", color = outcome),
                    n_rows = 20, size = 4, flip = TRUE,
                    family = "sans") + #dummy empty pictogram for legend
     geom_pictogram(color = shield_color, fill = "red",
                    aes(label = label_vacc ),
                    n_rows = 20, size = 7, flip = TRUE,
                    family = "EmojiOne", show.legend = F) + 
     
     coord_equal() +
     theme_enhance_waffle() +
     facet_wrap(~vacc) +
     scale_color_manual(name = "Resultado",
                        values = c(death_color, hosp_color,covid_color,person_color, "white"),
                        breaks = c("death", "hosp","infected", "none", 'white'),
                        labels = c("Morte","Hospitalização","Covid-19","Não Infectada","")) +
     
     guides(color  = guide_legend( override.aes = list(size = 10, label = "■")))+
     theme_void() + theme(legend.key = element_blank(), legend.position="bottom")
   
  )
}


deprecated_output_plot_vacc_non_vacc_covid_sort <- renderPlot({
  #TO-DO:GRAFICO de percentual em cada população
  #TO-DO:NEAT
  
  get_bar_vacc_inf_neat <- function(n_inf) {
    
    points_dist = 100/sqrt(population_size) 
    
    points_per_col = round(sqrt(n_inf)) 
    
    row_number = 1:n_inf
    
    return(tibble(x_bar = points_dist * (floor((row_number-1) / points_per_col)),
                  y_bar = points_dist * ((row_number-1) %% points_per_col))
    )
    
    
  }
  
  get_bar_vacc_n_inf_neat <- function(n_inf, n_vacc) {
    
    points_dist = 100/sqrt(population_size) 
    
    points_per_col = round(sqrt(population_size)) 
    
    row_number = 1:(n_vacc-n_inf)
    
    return(tibble(x_bar = points_dist * (floor((row_number-1) / points_per_col)),
                  y_bar = points_dist * ((row_number-1) %% points_per_col))
    )
    
    
  }
  
  
  get_bar_vacc_inf <- function(n, vacc_per, prob) {
    x_len = vacc_per * sqrt(prob)
    x1 = (vacc_per-x_len)/2
    x2 = x1 + x_len
    y_len =  100 * sqrt(prob)
    y1 = (100-y_len)/2
    y2 = y1 + y_len
    
    return(tibble(x_bar = runif(n)*x_len + x1,
                  y_bar = runif(n)*y_len + y1))
  }
  
  get_bar_vacc_n_inf <- function(n, vacc_per, prob) {
    x_len = vacc_per * sqrt(prob)
    x1 = (vacc_per-x_len)/2
    x2 = x1 + x_len
    y_len =  100 * sqrt(prob)
    y1 = (100-y_len)/2
    y2 = y1 + y_len
    
    bar <- tibble(x_bar = runif(n)*vacc_per,
                  y_bar = runif(n)*100) %>%
      mutate(invalid = x_bar > x1 & x_bar < x2 & y_bar > y1 & y_bar < y2)
    while(nrow(bar %>% filter(invalid))){
      bar <- bar %>%
        mutate(x_bar = ifelse(invalid, runif(n)*vacc_per,x_bar),
               y_bar = ifelse(invalid, runif(n)*100, y_bar)) %>%
        mutate(invalid =  x_bar > x1 & x_bar < x2 & y_bar > y1 & y_bar < y2)
    }
    return(bar)
  }
  
  model = get_infected_2_death_model()
  summ_pop <- population_outcome() %>% 
    group_by(vacc, infected) %>%
    summarise(n = n())
  
  vacc_per = input$vacc_per_a
  
  bar_vacc_inf = get_bar_vacc_inf_neat(n_inf = (summ_pop %>% filter (vacc,infected))$n) 
  bar_vacc_n_inf = get_bar_vacc_n_inf_neat(n_inf = (summ_pop %>% filter (vacc,infected))$n,
                                           n_vacc =sum( (summ_pop %>% filter (vacc))$n)) 
  #NOT NEAT CODE - DO NOT DELETE
  # bar_vacc_inf = get_bar_vacc_inf(n = (summ_pop %>% filter (vacc,infected))$n,
  #                                 vacc_per = vacc_per, 
  #                                 prob = model[["infected_if_vacc_prob"]]) 
  # 
  # bar_vacc_n_inf = get_bar_vacc_n_inf(n = (summ_pop %>% filter (vacc,!infected))$n,
  #                                     vacc_per = vacc_per, 
  #                                     prob = model[["infected_if_vacc_prob"]]) 
  # 
  
  population_outcome() %>%
    mutate(x_bar = x_neat, y_bar = y_neat) %>% 
    group_by(vacc, infected) %>%
    mutate( x_bar = ifelse(vacc & infected, bar_vacc_inf$x_bar, x_bar),
            y_bar = ifelse(vacc & infected, bar_vacc_inf$y_bar, y_bar)) %>%
    mutate( x_bar = ifelse(vacc & !infected, bar_vacc_n_inf$x_bar, x_bar),
            y_bar = ifelse(vacc & !infected, bar_vacc_n_inf$y_bar, y_bar)) %>%
    ungroup() %>%
    ggplot(aes(x=x_bar, y=y_bar)) +
    
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
  
})



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
