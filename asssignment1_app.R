library(shiny)
library(tidyverse)
library(plotly)

resources <- read_csv("natural-resources.csv")



ui <- fluidPage(

  titlePanel("Analysis of Natural Resources of Different Countries"),

  h3("1.Resource production of different countries"),

  fluidRow(
    selectInput("country","Select a country",choices = unique(resources$Entity)),
    selectInput("res","Select a resource", choices = c("Oil","Natural Gas","Coal")),

  mainPanel(
    plotOutput("plot1")
  )),




  h3("2.Resource consumption of different countries"),

  fluidRow(
    selectInput("country","Select a country",choices = unique(resources$Entity)),
    selectInput("res","Select a resource", choices = c("Oil","Natural Gas","Coal")),

  mainPanel(
    plotOutput("plot2")
  )

))





server <- function(input, output, session) {

}

shinyApp(ui, server)

