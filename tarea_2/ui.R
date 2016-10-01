library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
theme_set(theme_gray())

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Integración por Monte Carlo"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      textInput("funcion", 
                   "Función a integrar:",
                   value = "x**3"),
      numericInput("simul",
                   "Número de simulaciones:",
                   value = 10000,
                   min = 10,
                   max = 100000),
      numericInput("a",
                   "Límite inferior:",
                   value = -2),
      numericInput("b",
                   "Límite superior:",
                   value = 2),
      numericInput("alphas",
                   "Significancia (α):",
                   value = 0.05)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Función a integrar"),
      plotOutput("graf_fun"),
      h2("Resultado de la Integral por Monte Carlo:"),
      textOutput("text1"),
      h2("Intervalos de confianza"),
      plotOutput("graf_conf")
    )
  )
))