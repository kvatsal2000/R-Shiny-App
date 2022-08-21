library(shiny)
library(tidyverse)
library(plotly)

resources <- read_csv("natural-resources.csv") %>%
  select(`Gas production`,`Gas consumption`,`Coal production`,`Coal consumption`,`Oil production`,`Oil consumption`,
         `Entity`,`Year`,`Gas reserves`,`Oil reserves`,`Coal reserves`)




resources2 <- read_csv("natural-resources.csv") %>%
  select(`Gas production`,`Gas consumption`,`Coal production`,`Coal consumption`,`Oil production`,`Oil consumption`,`Entity`,`Year`,`Gas reserves`,`Oil reserves`,`Coal reserves`,`Population`) %>%
  pivot_longer(cols = `Gas production`:`Oil consumption`,
               names_to = 'type',
               values_to = 'quantity' )


resources3 <- resources2 %>%
  select(`Entity`,`Year`,`Gas reserves`,`Oil reserves`,`Coal reserves`) %>%
  pivot_longer(cols = `Gas reserves`:`Coal reserves`,
               names_to = 'reserve_type',
               values_to = 'quantity_reserve')


ui <- fluidPage(

  titlePanel("ANALYSIS OF DIFFERENT TYPES NATURAL RESOURCES FOR DIFFERENT COUNTRIES"),

  h3("1.Resource production of different countries"),

  fluidRow(
    selectInput("country","Select a country",choices = unique(resources2$Entity)),
    selectInput("res","Select a resource", choices = c("Oil production","Gas production","Coal production")),

  mainPanel(
    plotlyOutput("plot1")
  )),




  h3("2.Resource consumption of different countries and comparison with population"),

  fluidRow(
    selectInput("country2","Select a country",choices = unique(resources2$Entity)),
    selectInput("res2","Select a resource", choices = c("Oil consumption","Gas consumption","Coal consumption")),

  mainPanel(
    plotlyOutput("plot2"),
    plotlyOutput("plot4")
  )

),



h3("3.Reserve Estimates of different types of resources for various countries with year "),

fluidRow(
  selectInput("country3","Select a country",choices = unique(resources3$Entity)),
  selectInput("res3","Select a resource", choices = c("Oil reserves","Gas reserves","Coal reserves")),

  mainPanel(
    plotlyOutput("plot3")
  )

),
includeCSS("styles.css")
)





server <- function(input, output, session) {

  output$plot1 <- renderPlotly({
  rec1 <- resources2 %>%
      filter(Entity ==  input$country) %>%
    filter( type == input$res)

   p<- ggplot(rec1,aes(x = `Year`,
                         y= quantity/1000000)) + geom_line(stat = 'identity') + theme_dark() +
     scale_x_continuous( breaks = seq(min(resources2$Year),
                                        max(resources2$Year), by=2))  +
     labs(x = "Year", y = "Production(in million)")
   ggplotly(p)

  })



  output$plot2 <- renderPlotly({
    rec2 <- resources2 %>%
      filter(Entity ==  input$country2) %>%
      filter( type == input$res2)

    p<- ggplot(rec2,aes(x = `Year`,
                        y= quantity/1000000)) + geom_line(stat = 'identity') +
      theme_dark() +
      scale_x_continuous( breaks = seq(min(resources2$Year),
                                        max(resources2$Year), by=2)) +
      labs(x = "Year", y = "Consumption (in million)")



  })


  output$plot4 <- renderPlotly({
    rec2 <- resources2 %>%
      filter(Entity ==  input$country2) %>%
      filter( type == input$res2)

    p<- ggplot(rec2,aes(x = `Year`,
                        y= Population/1000000)) + geom_line(stat = 'identity') +
      theme_dark() +
      scale_x_continuous( breaks = seq(min(resources2$Year),
                                       max(resources2$Year), by=2)) +
      labs(x = "Year", y = "Population(in millions)")



  })



  output$plot3 <- renderPlotly({
    rec3 <- resources3 %>%
      filter(Entity ==  input$country3) %>%
      filter( reserve_type == input$res3)

    p<- ggplot(rec3,aes(x = `Year`,
                        y= quantity_reserve/1000000)) + geom_line(stat = 'identity') +
      theme_dark() +
      scale_x_continuous( breaks = seq(min(resources2$Year),
                                       max(resources2$Year), by=2)) +
      labs(x = "Year", y = "Reserves(in millions)")
    ggplotly(p)

  })

}

shinyApp(ui, server)

