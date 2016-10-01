library(shiny)
library(dplyr)
library(plotly)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Método de la función inversa"),
  headerPanel("Simulación de una distribución exponencial"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("simula",
                  "Número de simulaciones:",
                  min = 10,
                  max = 10000,
                  value = 1000),
      numericInput("lambda", 
                   "Parámetro λ:",
                  #min = 0.01,
                  #max = 100,
                  value = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Histograma de la simulación:"),
      plotlyOutput("trendPlot"),
      h2("Prueba Smirnov-Kolmogorov:"),
      textOutput("text1")
    )
  )
))