shinyServer(function(input, output) {
  
  Finv <- function(u, lambda) {return(-log(1-u)/lambda)}
  set.seed(20160817)
  
  ElInput <- reactive({
    nsim <- input$simula
    lambda <- input$lambda
    U <- runif(nsim)
    X <- Finv(U, lambda)
  })
  
  Comp <- reactive({
    Y <- rexp(input$simula,rate=input$lambda)
  })
  
  output$trendPlot <- renderPlotly({
    
    X <- ElInput()

    X2 <- seq(0,max(X),(max(X)-0)/input$simula)
    funcion <- function(x) input$lambda * exp(- (x * input$lambda)) * input$simula/(input$lambda*10)
    aplicada <- sapply(X2, funcion)
    
     plot_ly(x=X,type="histogram", opacity=0.4, name = "Simulación por función inversa") %>%
      add_trace(x=X2, y=aplicada, type="lines", opacity=1, name = "PDF")
  })
  
  output$text1 <- renderPrint({

    X <- ElInput()
    Y <- Comp()
    KS <- ks.test(X, Y)
    
    KS
  })
  
})
