#05 - PLOT VACC X N VACC X HOSP x DEATH
plot_vacc_n_vacc_sim  <- function(data){
  data %>%
    ggplot(aes(x=x_neat, y=y_neat))+
    geom_point(shape = 1, alpha = 0, size = 1, aes(color = outcome)) +
    geom_text(aes(color = outcome,label = label),family = "fontawesome-webfont",
              size = point_size, show.legend = FALSE)+
    
    geom_point(shape = 1, aes(alpha = vacc), size = shield_size, color = shield_color) +
    
    scale_color_manual(name = "Resultado",
                       values = c(death_color, hosp_color,covid_color,person_color),
                       breaks = c("death", "hosp","infected", "none"),
                       labels = c("Morte","Hospitalização","Covid-19","Não Infectada")) +
    guides(color  = guide_legend(override.aes =
                                   list(shape = 15,
                                        #fill=color, #c(death_color, hosp_color,covid_color,person_color),
                                        alpha = 1, size = 5),
                                 nrow=2,byrow=TRUE)) +
    scale_alpha_manual(name = element_blank(),
                       values = c(1,0),
                       breaks = c(T,F),
                       labels = c("Vacina",element_blank())) +
    guides(alpha  = guide_legend(override.aes = list(color = "black")),
           nrow=2, byrow=TRUE) +
    
    theme_void() + theme(legend.position="bottom")
}
