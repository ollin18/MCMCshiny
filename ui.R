concre <- read.csv("./Data/concreto.csv")

library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(ggplot2)
library(markdown)
library(DT)
library(grid)
library(gridExtra)

shinyUI(fluidPage(theme = shinytheme("united"),
  navbarPage("Tareas",
             tabPanel("Yo",
                      mainPanel(
                        h1("Ollin Demian Langle Chimal"),
                        h3("Cuenta:"),
                        h3("116754"),
                        img(src="mcfly.jpg",heigth=800,width=800,align = "right")
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
                                    value = "x**3 + x**2 * x + 5 * cos(x) + tanh(x)"),
                          numericInput("simul",
                                       "Número de simulaciones:",
                                       value = 1000,
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
             ),
             tabPanel("Tarea 4",
                      sidebarPanel(
                        h1("Aquí se muestra la tabla de datos a utilizar."),
                        h3("Se trata de la resistencia del concreto"),
                       
                        selectInput("dataset", "Datos a utilizar:", 
                                    choices = c("concreto")),
                      
                        fluidRow(
                          column(3, selectInput("X",
                                               "Independiente:",
                                               c(unique(as.character(names(concre)))))),
                          column(3, selectInput("Y",
                                                "Dependiente:",
                                                c(rev(unique(as.character(names(concre)))))))
                        )
                        
                        ),
                      
                      mainPanel(
                  
                        fluidRow(
                          DT::dataTableOutput("table"),
                          plotOutput("disperso"),
                          h2("DISTRIBUCIONES A PRIORI"),
                          h4("Lo hago de esta manera porque no sé nada de cemento"),
                          h4("alpha con distribución a priori Normal"),
                          plotOutput("distribua"),
                          h4("beta con distribución a priori Normal"),
                          plotOutput("distribub"),
                          h4("Ya me acostumbré a usar precisión en lugar de la varianza"),
                          h4("tau con distribución a priori Gamma"),
                          plotOutput("distribut")
                        )
                      )
                      
                      )
  )
  
))