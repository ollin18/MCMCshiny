library(shiny)
library(dplyr)
library(plotly)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tarea 1 de Estadística Computacional"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("simul",
                  "Número de simulaciones:",
                  min = 10,
                  max = 10000,
                  value = 1000),
      sliderInput("lambda", 
                   "Parámetro λ:",
                  min = 0.01,
                  max = 1,
                  value = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("trendPlot"),
      
      textOutput("text1")
    )
  )
))