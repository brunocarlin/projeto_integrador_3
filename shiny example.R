library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)

injuries <- vroom::vroom("data/injuries.tsv.gz")

head(injuries)


products <- vroom::vroom("data/products.tsv")
head(products)

population <- vroom::vroom("data/population.tsv")
head(population)

header <- dashboardHeader(title = "NEISS - Registros de Acidentes nos EUA (2017)",
                          titleWidth = 450)

sidebar <- dashboardSidebar()


body <- dashboardBody(
  fluidRow(
    box(width = 4,
        solidHeader = TRUE,
        title = "Motivo do acidente",
        status = "warning",collapsible = TRUE,
        selectInput("code", NULL, choices = setNames(products$prod_code, products$title))),
    box(width = 4,
        solidHeader = TRUE,
        title = "Quantos resultados?",
        status = "primary",
        sliderInput("number", NULL, value = 5, min = 1, max = 10)),
    box(width = 4,
        solidHeader = TRUE,
        title = "O que deseja visualizar?",
        status = "success",
        radioButtons("var", NULL,
                     choiceNames = list("Diagnóstico", "Parte do Corpo", "Local do Acidente"),
                     choiceValues = list("diag", "body_part", "location")))
  ),
  fluidRow(
    valueBoxOutput("titulo", width = 12),
  ),
  conditionalPanel(
    condition = "input.var == 'diag'",
    fluidRow(
      box(width = 3,
          tableOutput(outputId = "diag1")),
      column(width = 9,
             plotOutput(outputId = "diag2"))
    )
  ),
  
  conditionalPanel(
    condition = "input.var == 'body_part'",
    fluidRow(
      box(width = 4,
          tableOutput(outputId = "body1")),
      column(width = 8,
             plotOutput(outputId = "body2", width = "100%", height = "400px"))
    )
  ),
  
  conditionalPanel(
    condition = "input.var == 'location'",
    fluidRow(
      box(width = 4,
          tableOutput(outputId = "location1")),
      column(width = 8,
             plotOutput(outputId = "location2"))
    )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output, session){
  
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$titulo <- renderValueBox(
    valueBox(paste("Top", input$number, "resultados"), subtitle = NULL, icon = NULL,
             color = "purple"
    )
  )
  
  
  output$diag1 <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      rename(Diagnóstico = diag, Taxa = n)
  )
  
  output$diag2 <- renderPlot(
    selected() %>% count(diag, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      ggplot(aes(x = reorder(diag,n), y = n)) +
      geom_col(fill = "lightblue", width = 0.8, alpha = 0.8) +
      scale_y_log10() +
      labs(y = "Taxa por 10mil pacientes",
           x = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank())
  )
  
  output$body1 <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      rename(Membro = body_part, Taxa = n)
  )
  
  output$body2 <- renderPlot(
    selected() %>% count(body_part, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      ggplot(aes(x = reorder(body_part,n), y = n)) +
      geom_col(fill = "lightblue", width = 0.8, alpha = 0.8) +
      scale_y_log10() +
      labs(y = "Taxa por 10mil pacientes",
           x = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank())
  )
  
  output$location1 <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      rename(Local = location, Taxa = n))
  
  output$location2 <- renderPlot(
    selected() %>% count(location, wt = weight, sort = TRUE) %>% top_n(n = input$number, wt = n) %>% 
      ggplot(aes(x = reorder(location,n), y = n)) +
      geom_col(fill = "lightblue", width = 0.8, alpha = 0.8) +
      scale_y_log10() +
      labs(y = "Taxa por 10mil pacientes",
           x = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank())
  )
}

shinyApp(ui, server)
