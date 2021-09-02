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
