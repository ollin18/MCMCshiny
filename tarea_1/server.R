shinyServer(function(input, output) {
  
  Finv <- function(u, lambda) {return(-log(1-u)/lambda)}
  set.seed(20160817)
  
  ElInput <- reactive({
    nsim <- input$simul
    lambda <- input$lambda
    U <- runif(nsim)
    X <- Finv(U, lambda)
  })
  
  Comp <- reactive({
    Y <- rnorm(input$simul)
  })
  
  output$trendPlot <- renderPlotly({
    
    X <- ElInput()

    #X2 <- rexp(nsim, rate=lambda)
    
    plot_ly(x=X, type="histogram", opacity=0.6) #%>%
     #add_trace(x=X2, type="histogram", opacity=0.6)
     
  })
  
  output$text1 <- renderPrint({

    X <- ElInput()
    Y <- Comp()
    KS <- ks.test(X, Y)
    
    KS
  })
  
})
