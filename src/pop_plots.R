
#01 - PLOT VACC X N VACC
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

#02 - PLOT VACC X N VACC X COVID
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


#03 - PLOT VACC X N VACC X COVID ONLY WAFFLE
plot_covid_only_waffle <- function(data){
  return(data %>%
           filter(infected) %>%    #plot_covid_only()
           mutate(label_vacc = ifelse(vacc, emoji('white_circle'),"")) %>%
           mutate(vacc = factor(vacc, 
                                levels = c(T,F), 
                                labels = c("Vacinada","Não vacinada"))) %>%
           select(vacc, label, label_vacc, infected) %>%
           group_by(vacc, label, label_vacc,infected) %>%
           summarise(n = n()) %>%
           ggplot(aes( values = n))  +
           geom_pictogram(aes(label = label, color = infected),
                          n_rows = 20, size = 4, flip = TRUE,
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
                              values = c(covid_color,person_color),
                              breaks = c(T, F),
                              labels = c("Covid-19","Não Infectada")) +
           guides(color  = guide_legend( override.aes = list(size = 10, label = "■")))+
           theme_void() + theme(legend.key = element_blank(), legend.position="bottom")
  )}


#04 - PLOT VACC X N VACC X COVID WAFFLE
plot_vacc_non_vacc_covid_sort  <- function(data){
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
                          n_rows = 20, size = 4, flip = TRUE,
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

#05 - PLOT VACC X N VACC X HOSP x DEATH
plot_hosp_death  <- function(data){
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
                                   list(shape = shape_guide,
                                        fill=c(death_color, hosp_color,
                                               covid_color,person_color),
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

#06 - PLOT VACC X N VACC X HOSP x DEATH ONLY
plot_hosp_death_only <- function(data){
  return(data %>%
           filter(hosp | death) %>%    
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
                              values = c(death_color, hosp_color,covid_color,person_color),
                              breaks = c("death", "hosp","infected", "none"),
                              labels = c("Morte","Hospitalização","Covid-19","Não Infectada")) +
           
           guides(color  = guide_legend( override.aes = list(size = 10, label = "■")))+
           theme_void() + theme(legend.key = element_blank(), legend.position="bottom")
  )}
