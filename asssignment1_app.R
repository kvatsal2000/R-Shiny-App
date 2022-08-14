library(shiny)
library(tidyverse)
library(plotly)

resources <- read_csv("natural-resources.csv") %>%
  select(`Gas production`,`Gas consumption`,`Coal production`,`Coal consumption`,`Oil production`,`Oil consumption`,
         `Entity`,`Year`,`Gas reserves`,`Oil reserves`,`Coal reserves`)




resources2 <- read_csv("natural-resources.csv") %>%
  select(`Gas production`,`Gas consumption`,`Coal production`,`Coal consumption`,`Oil production`,`Oil consumption`,`Entity`,`Year`,`Gas reserves`,`Oil reserves`,`Coal reserves`) %>%
  pivot_longer(cols = `Gas production`:`Oil consumption`,
               names_to = 'type',
               values_to = 'quantity' )



ui <- fluidPage(

  titlePanel("Analysis of Natural Resources of Different Countries"),

  h3("1.Resource production of different countries"),

  fluidRow(
    selectInput("country","Select a country",choices = unique(resources2$Entity)),
    selectInput("res","Select a resource", choices = c("Oil production","Gas production","Coal production")),

  mainPanel(
    plotlyOutput("plot1")
  )),




  h3("2.Resource consumption of different countries"),

  fluidRow(
    selectInput("country2","Select a country",choices = unique(resources2$Entity)),
    selectInput("res2","Select a resource", choices = c("Oil consumption","Gas consumption","Coal consumption")),

  mainPanel(
    plotlyOutput("plot2")
  )

)
)





server <- function(input, output, session) {

  output$plot1 <- renderPlotly({
  rec1 <- resources2 %>%
      filter(Entity ==  input$country) %>%
    filter( type == input$res)

   p<- ggplot(rec1,aes(x = `Year`,
                         y= quantity)) + geom_line(stat = 'identity')
   ggplotly(p)

  })



  output$plot2 <- renderPlotly({
    rec2 <- resources2 %>%
      filter(Entity ==  input$country2) %>%
      filter( type == input$res2)

    p<- ggplot(rec2,aes(x = `Year`,
                        y= quantity)) + geom_line(stat = 'identity')
    ggplotly(p)

  })

}

shinyApp(ui, server)

