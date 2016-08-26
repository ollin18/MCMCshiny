shinyServer(function(input, output) {
  
  output$trendPlot <- renderPlotly({
    Finv <- function(u, lambda) {return(-log(1-u)/lambda)}
    
    set.seed(20160817)
    nsim <- input$simul
    lambda <- input$lambda
    
    U <- runif(nsim)
    
    X <- Finv(U, lambda)
    
    #X2 <- rexp(nsim, rate=lambda)
    
     plot_ly(x=X, type="histogram", opacity=0.6) #%>%
      #add_trace(x=X2, type="histogram", opacity=0.6)
     
  })
  
})
