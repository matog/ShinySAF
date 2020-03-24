# Basado en https://deanattali.com/blog/building-shiny-apps-tutorial/
# y en https://mastering-shiny.org/


library(shiny)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(tidyquant)
library(knitr)
library(dslabs)
library(janitor)

base <- read_csv2("base.csv")
# base$cod_inc=as.factor(base$cod_inc)
# base$cod_prog=as.factor(base$cod_prog)
# base$cod_serv=as.factor(base$cod_serv)
# base$cod_act=as.factor(base$cod_act)



ui <- fluidPage(
    headerPanel("SAF322 - Información prespuestaria"),
    fluidRow(
        column(4,
               selectInput("programa", "Programa", choices = unique(base$desc_prog))
               ),
        column(4,
               selectInput("actividad", "Actividad", choices = NULL)
               ),
        column(4,
               
               selectInput("inciso", "Inciso", choices = NULL),
               )
        ),
    plotOutput("grafico"),
    br(), br(),
    tableOutput("tabla")
)

server <- function(input, output, session) {
    programa <- reactive({
        filter(base, desc_prog == input$programa)
    })
    observeEvent(programa(), {
        choices <- unique(programa()$desc_apertura)
        updateSelectInput(session, "actividad", choices = choices) 
    })

    actividad <- reactive({
        req(input$actividad)
        filter(programa(), desc_apertura == input$actividad)
    })
    observeEvent(actividad(), {
        choices <- unique(actividad()$desc_inc)
        updateSelectInput(session, "inciso", choices = choices)
    })
    
    output$tabla <- renderTable({
        req(input$inciso)
        actividad() %>% 
            filter(desc_inc == input$inciso) %>% 
            select("Programa" = desc_prog, "Actividad" = desc_apertura, "Inciso" = desc_inc, "Crédito Vigente" = cred_vig, "Crédito Comprometido" = cred_comp, "Crédito Devengado" = cred_dev,)
    })

    output$grafico <- renderPlot({
        actividad() %>% 
            filter(desc_prog == input$programa) 
                    ggplot(actividad(), aes(fill=desc_inc, y=cred_vig, x=desc_inc)) + geom_bar(stat="identity")
        
        })
        
}

shinyApp(ui = ui, server = server)