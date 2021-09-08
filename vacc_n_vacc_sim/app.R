#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#library(plyr)
library(dplyr)
library(emojifont)
#library(waffle)
source("pop_gen_fix.R")
source("sim_plot.R")

##################################### parameters #################################
population_size <<- 784
vacc_per_init = 33
#viz parameters
point_size <<- 4   #glyph size - person
shield_size <<- 6  #glyph size - circle

shape_guide <<- 22

person_color <<- "grey"
death_color <<- "black"
hosp_color <<- "#58a758" #green
covid_color <<- "#bd4242" #"red
shield_color <<- "black"


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "sandstone.min.css",
                # Application title
                titlePanel("Covid - Simulação" ),
                tags$a(href="https://twitter.com/robertodepinho", "@robertodepinho"),
                tags$a(href="https://ascoisas.com/2021/covid/", "( SOBRE @ ASCOISAS.COM)"),
                
                
                sliderInput("vacc_per", 
                            "Percentual vacinado da população:", 
                            min = 0, max = 100, 
                            value = vacc_per_init),
                plotOutput("plot_vacc_n_vacc_sim", height = 600),
                hr(),
                HTML('<h2>notas</h2>
<div id="section-simulação" class="section level4">
<h4>Simulação</h4>
<p>A simulação da população e resultados foi feita de acordo com os parâmetros do artigo abaixo. O artigo está em <a href="https://pt.wikipedia.org/wiki/Pr%C3%A9-publica%C3%A7%C3%A3o">preprint</a>. Foi utilizada a sua versão 2.</p>
<p><strong>Influence of age on the effectiveness and duration of protection in Vaxzevria and CoronaVac vaccines</strong> Thiago Cerqueira-Silva, Vinicius de Araújo Oliveira, Julia Pescarini, Juracy Bertoldo Júnior, Tales Mota Machado, Renzo Flores-Ortiz, Gerson Penna, Maria Yury Ichihara, Jacson Venâncio de Barros, Viviane S. Boaventura, Mauricio L. Barreto, Guilherme Loureiro Werneck, Manoel Barral-Netto medRxiv 2021.08.21.21261501; doi: <a href="https://doi.org/10.1101/2021.08.21.21261501">https://doi.org/10.1101/2021.08.21.21261501</a> <em>This article is a preprint and has not been peer-reviewed. It reports new medical research that has yet to be evaluated and so should not be used to guide clinical practice.</em></p>
<p>Considera-se que todos os indivíduos foram expostos por tempo suficiente para que sejam infectados, caso sem vacina.</p>
<p>A partir dos valores da Tabela 1, são feitos sorteios para marcar cada indivíduo com <em>Infection</em>, <em>Hospitalization</em>, <em>ICU admission</em> e <em>Death</em>, considerando o status de vacinado ou não. São considerados apenas os valores para <strong>Vaxzevria/Fiocruz (“<em>AstraZeneca</em>”)</strong> / <strong>Fully vaccinated (Completamente Vacinados)</strong>.</p>
</div>
<div id="section-créditos" class="section level4">
<h4>Créditos</h4>
<ul>
<li><p>Concepção: <a href="https://twitter.com/robertodepinho">Roberto de Pinho</a></p></li>
<li><p>Desenvolvimento: <a href="https://github.com/robertodepinho">Roberto de Pinho</a>, <a href="https://github.com/mmfava">Marília Melo Favalesso</a> &amp; <a href="https://github.com/tomasbarcellos">Tomás Barcellos</a></p></li>
<li><p>Agradecimentos: <a href="https://cidacs.bahia.fiocruz.br/en/team/manoel-barral-netto-2/">Manoel Barral Netto - Fiocruz</a></p></li>
<li><p>Código fonte: <a href="https://github.com/robertodepinho/vacc_non_vacc">vacc_non_vacc</a></p></li>')
                
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    population_outcome <- reactive({
        #vacc_per = ival() #input$vacc_per_a
        return(population_by_vacc_per(input$vacc_per))
    })
    
    output$plot_vacc_n_vacc_sim <- renderPlot({
        population_outcome() %>%
            plot_vacc_n_vacc_sim()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
