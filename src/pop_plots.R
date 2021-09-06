
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
    guides(color  = guide_legend(override.aes = list(shape = 15,
                                                     #fill=c(covid_color,person_color),
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
  
  summ_vacc <- data %>%
    filter(infected) %>%  
    group_by(vacc)%>%
    summarise(n = n()) %>%
    mutate( per = round(n / sum(n) * 100))
  
  max_n_row = floor(sqrt(max(summ_vacc$n)))+1
  
  data %>%
    filter(infected) %>%    #plot_covid_only()
    mutate(label_vacc = ifelse(vacc, emoji('white_circle'),"")) %>%
    mutate(vacc = factor(vacc, 
                         levels = c(T,F), 
                         labels = c(paste("Vacinada", (summ_vacc %>% filter(vacc))$per,"%"),
                                    paste("Não vacinada", (summ_vacc %>% filter(!vacc))$per,"%")))) %>%
    select(vacc, label, label_vacc, infected) %>%
    group_by(vacc, label, label_vacc,infected) %>%
    summarise(n = n()) %>%
    ggplot()  +
    geom_pictogram(aes(values = n, label = label, color = infected),
                   n_rows = max_n_row, 
                   size = point_size, 
                   flip = TRUE,
                   family = "fontawesome-webfont", show.legend = F) +
    geom_pictogram(aes(values = n, label = " ", color = infected),
                   n_rows = max_n_row, 
                   size = point_size, 
                   flip = TRUE,
                   family = "sans") + #dummy empty pictogram for legend
    geom_pictogram(color = shield_color, fill = "red",
                   aes(values = n, label = label_vacc ),
                   n_rows = max_n_row, 
                   size = point_size, 
                   flip = TRUE,
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
  
}


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

#06 - PLOT VACC X N VACC X HOSP x DEATH ONLY
plot_hosp_death_only <- function(data){
  
  summ_vacc <- data %>%
    filter(hosp | death) %>%  
    group_by(vacc)%>%
    summarise(n = n()) %>%
    mutate( per = round(n / sum(n) * 100))
  
  max_n_row = floor(sqrt(max(summ_vacc$n)))+1
  
  
  return(data %>%
           filter(hosp | death) %>%    
           mutate(label_vacc = ifelse(vacc, emoji('white_circle'),"")) %>%
           mutate(vacc = factor(vacc, 
                                levels = c(T,F), 
                                labels = c(paste("Vacinada", (summ_vacc %>% filter(vacc))$per,"%"),
                                           paste("Não vacinada", (summ_vacc %>% filter(!vacc))$per,"%")))) %>%
           select(vacc, label, label_vacc, outcome) %>%
           group_by(vacc, outcome, label, label_vacc) %>%
           summarise(n = n()) %>%
           ggplot(aes( values = n))  +
           geom_pictogram(aes(label = label, color = outcome),
                          n_rows = max_n_row, 
                          size = point_size, flip = TRUE,
                          family = "fontawesome-webfont", show.legend = F) +
           geom_pictogram(aes(label = " ", color = outcome),
                          n_rows = max_n_row, 
                          size = point_size, flip = TRUE,
                          family = "sans") + #dummy empty pictogram for legend
           geom_pictogram(color = shield_color, fill = "red",
                          aes(label = label_vacc ),
                          n_rows = max_n_row, 
                          size = point_size, flip = TRUE,
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


plot_score <- function(data) {
  
  data %>%
    group_by(outcome) %>%
    summarise(n = n()) %>%
    add_row(outcome = "none", n = 0) %>% #make sure there one row for each outcome
    add_row(outcome = "death", n = 0) %>% 
    add_row(outcome = "hosp", n = 0) %>% 
    add_row(outcome = "infected", n = 0) %>% 
    group_by(outcome) %>%
    summarise(n = sum(n)) %>%
    mutate(x = c(2,2,1,1), 
           y = c(1,2,1,2)) %>%
    mutate(label_out = factor(outcome, 
                              levels = c("death", "hosp","infected", "none"),
                              labels = c("Mortes","Hospitalizações","Covid-19","Não infectadas"))) %>%
    ggplot(aes(x = factor(x),y = factor(y))) +
    geom_tile(fill = "white") + 
    geom_point(aes(size = n, fill = outcome), shape = 21) + 
    scale_size(range = c(1, 20), limits = c(0,population_size)) + 
    geom_label(aes(label = paste(label_out,n,sep="\n"),
                   fill = outcome),
               color = c("orange", "black","black","black"),
               nudge_y = 0.4,
               size = 8) + 
    scale_fill_manual(name = "Resultado",
                       values = c(death_color, hosp_color,covid_color,person_color),
                       breaks = c("death", "hosp","infected", "none"),
                       labels = c("Morte","Hospitalização","Covid-19","Não Infectada")) + 
    theme_void() + theme(legend.position="none") 
  
  
  # facet_wrap(~outcome) +
  #   geom_point(x=10,y= 10, aes(size = n)) +
  #   scale_size_continuous(limits = c(0, population_size), 
  #                         range=c(0,80))
  # 
  # geom_rect(aes(xmin=x, ymin=y, 
  #               xmax= x + 30, ymax = y +10, 
  #               fill = outcome))+
  #   geom_text(x=15, y=5, show.legend = FALSE,
  #             aes(label = paste(label_out,n,sep="\n"), 
  #                 color = outcome),
  #             size = 8) +
  #   
  #   theme_void() + theme(legend.position="none", 
  #                        strip.background = element_blank(),
  #                        strip.text.x = element_blank()) +
  #   scale_fill_manual(name = "Resultado",
  #                     values = c(death_color, hosp_color,covid_color,person_color),
  #                     breaks = c("death", "hosp","infected", "none"),
  #                     labels = c("Morte","Hospitalização","Covid-19","Não Infectada")) +
  #   scale_color_manual(name = "Resultado",
  #                      values = c("orange", "black","black","black"),
  #                      breaks = c("death", "hosp","infected", "none"),
  #                      labels = c("Morte","Hospitalização","Covid-19","Não Infectada"))
  # 
  # 
  
}
