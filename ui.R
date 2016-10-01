library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(markdown)

shinyUI(fluidPage(
  navbarPage("Tareas",
             tabPanel("Yo",
                      mainPanel(
                        h1("Ollin Demian Langle Chimal"),
                        h3("Cuenta:"),
                        h3("116754")
                      )),
             tabPanel("Tarea 1",
                      titlePanel("Método de la función inversa"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("simula",
                                      "Número de simulaciones:",
                                      min = 10,
                                      max = 10000,
                                      value = 1000),
                          numericInput("lambda", 
                                       "Parámetro λ:",
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
             ),
             tabPanel("Tarea 2",
                      titlePanel("Integración por Monte Carlo"),
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
                        
                        mainPanel(
                          h2("Función a integrar"),
                          plotOutput("graf_fun"),
                          h2("Resultado de la Integral por Monte Carlo:"),
                          textOutput("text2"),
                          h2("Intervalos de confianza"),
                          plotOutput("graf_conf")
                        )
                      )
             )
  )
))