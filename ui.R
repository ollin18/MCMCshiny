concre <- read.csv("./Data/concreto.csv")

paquetines <- c("shiny","shinythemes","dplyr","plotly","ggplot2","markdown","DT","grid","gridExtra")
no_instalados <- paquetines[!(paquetines %in% installed.packages()[,"Package"])]
if(length(no_instalados)) install.packages(no_instalados)
lapply(paquetines, library, character.only = TRUE)
Rcpp::sourceCpp("mhMCMC.cpp")

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
             tabPanel("Tarea 4 y 5",
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
                        ),
                        numericInput("nCadenas", "Número de cadenas", value=1, min=1, max=5, step=1),
                        sliderInput("sLongitud", "Longitud de las cadenas", min=10000, max=1000000, value=250000),
                        sliderInput("sBurnin", "Burnin", min=100, max=50000, value=20000),
                        actionButton("button", "Just do it")  
                        
                        ),
                      
                      mainPanel(
                        tabsetPanel(type="tabs",
                                  tabPanel("Datos", 
                                      fluidRow(
                                        DT::dataTableOutput("table"),
                                        plotOutput("disperso"),
                                        h2("DISTRIBUCIONES A PRIORI"),
                                        h4("Lo hago de esta manera porque no sé nada de cemento"),
                                        h4("alpha con distribución a priori Normal"),
                                        plotOutput("distribua"),
                                        h4("beta con distribución a priori Normal"),
                                        plotOutput("distribub"),
                                        h4("sigma con distribución a priori Gamma"),
                                        plotOutput("distribut")
                                      )),
                                  tabPanel("Cadenas",
                                           fluidRow(
                                             column(8,DT::dataTableOutput("cadenasMCMC"))
                                           )),
                                  tabPanel("Histogramas",
                                           fluidRow(
                                             column(4, plotOutput("hist_alpha")),
                                             column(4, plotOutput("hist_beta")),
                                             column(4, plotOutput("hist_sigma")),
                                             h1("A priori en rosa y a posteriori en azul"),
                                             column(4, plotOutput("dens_alpha")),
                                             column(4, plotOutput("dens_beta")),
                                             column(4, plotOutput("dens_sigma")),
                                             h1("Series"),
                                             column(4, plotOutput("cadena_alpha")),
                                             column(4, plotOutput("cadena_beta")),
                                             column(4, plotOutput("cadena_sigma"))
                                           )),
                                  tabPanel("Valores",
                                           fluidRow(
                                             column(8,DT::dataTableOutput("valores")),
                                             plotOutput("ajuste")
                                           ))
                        )
                      )
                      
                      )
  )
  
))