shinyServer(function(input, output){
  #### Función importantísima para que todo funcione en la vida!!!! ###
  ':=' <- function(lhs, rhs) {
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
      lhs <- lhs[-1]
    if (length(lhs) == 1) {
      do.call(`=`, list(lhs[[1]], rhs), envir=frame)
      return(invisible(NULL)) 
    }
    if (is.function(rhs) || is(rhs, 'formula'))
      rhs <- list(rhs)
    if (length(lhs) > length(rhs))
      rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
    for (i in 1:length(lhs))
      do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
    return(invisible(NULL)) 
  }
  
  al_cero <- function(x){dist(c(x,0))}
  
  integral_MC <- function(f,a,b,N){
    
    simulacion <- runif(N,a,b)
    
    funcion_aplicada <- f(simulacion)
    minimo <- min(funcion_aplicada)
    maximo <- max(funcion_aplicada)
    comparada <- runif(N,min(minimo,0),max(maximo,0))
    
    dist_fun <- sapply(funcion_aplicada,al_cero)
    dist_com <- sapply(comparada,al_cero)
    
    bajo_la_curva <- sum(dist_fun >= dist_com & (sign(comparada)==1 & sign(funcion_aplicada)==1))
    sobre_la_curva <- sum(dist_fun >= dist_com & (sign(comparada)==-1 & sign(funcion_aplicada)==-1))
    (bajo_la_curva - sobre_la_curva)/N * abs(max(maximo,0)-min(minimo,0))*abs(b-a)
  }
  
  fun <- reactive({
    texto <- paste("aux <- function(x) ",input$funcion)
    eval(parse(text = texto))
    aux
  })
  
  ElInput <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$simul
    f     <- fun()
    alphas <- input$alphas
    return(list(a,b,N,f,alphas))
  })
  
  LaIntegral <- reactive({
    c(a,b,N,f,alphas) := ElInput()
    int <- integral_MC(f,a,b,N)
    int
  })
  
  confianza <- function(Numero){
    c(a,b,N,f,alphas) := ElInput()
    uniforme <- runif(Numero, min = a, max = b)
    aplicada <- (b-a)*f(uniforme)
    media <- mean(aplicada)
    desvia <- qnorm(alphas/2, lower.tail = F) * sd(aplicada)/sqrt(Numero)
    minimo <- media - desvia
    maximo <- media + desvia
    lista <- list(media,minimo,maximo,Numero)
    df <- data.frame(lista)
    names(df) <- paste(c("media","minimo","maximo","Numero"))
    df
  }
  
  Intervalos <- reactive({
    c(a,b,N,f,alphas) := ElInput()
    repeticiones <- seq(10,N,30)
    sapply(repeticiones,confianza, simplify = FALSE) %>%
      bind_rows()
  })
  
  output$text1 <- renderText({
    
    integral <- round(LaIntegral(), 5)
    
    
    as.character(integral)
  })
  
  output$graf_fun <- renderPlot({
    c(a,b,N,f,alphas) := ElInput()
    x <- seq(a,b,(b-a)/N)
    y <- f(x)
    df <- data.frame(x,y)

    ggplot(df,aes(x,y))+
      geom_line(colour="#990000",size=1.5)+
      geom_area(fill="#06b9C7",alpha=0.3)+
      ylab('f(x)')
  })
  
  output$graf_conf <- renderPlot({
    c(a,b,N,f,alphas) := ElInput()
    ggplot(Intervalos(), aes(x = Numero, y = media)) + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), 
                  alpha =0.4, fill = '#06b9C7') + 
      geom_line(color = 'black', size = 0.6) + 
      ylab('Valor de la integral MC') + 
      xlab("Número de simulaciones")+ 
      ggtitle("Intervalos de Confianza")
  })
  
})












