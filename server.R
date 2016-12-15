concre <- read.csv("./Data/concreto.csv")

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(markdown)
library(DT)
library(grid)
library(gridExtra)


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


shinyServer(function(input, output){

  
  ###########################################################################
  ###########################################################################
  #################                                         #################
  #################             Tarea      1                #################
  #################                                         #################
  ###########################################################################
  ###########################################################################
  
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
      add_trace(x=X2, y=aplicada, type="scatter", opacity=0.3, name = "PDF")
  })
  
  output$text1 <- renderPrint({

    X <- ElInput()
    Y <- Comp()
    KS <- ks.test(X, Y)

    KS
  })
  
  ###########################################################################
  ###########################################################################
  #################                                         #################
  #################             Tarea      2                #################
  #################                                         #################
  ###########################################################################
  ###########################################################################
  
  al_cero <- function(x){dist(c(x,0))}
  
  
  #### Esta función no la voy a usar pero la dejo aquí por si algún día alguien la ve y le gusta ###
  #### Es integración por Montecarlo Straightforward ####
  #### Tal cuál checamos el número de puntos que caen dentro de la curva ####
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
  
  ElInput2 <- reactive({
    a     <- input$a
    b     <- input$b
    N     <- input$simul
    f     <- fun()
    alphas <- input$alphas
    return(list(a,b,N,f))
  })
  
  ElInputalphas <- reactive({
    alphas <- input$alphas
    return(alphas)
  })
 
  LaIntegral <- reactive({
    alphas <- ElInputalphas()
    c(a,b,N,f) := ElInput2()
    int <- integral_MC(f,a,b,N)
    int
  })
  
  confianza <- function(Numero){
    alphas <- ElInputalphas()
    c(a,b,N,f) := ElInput2()
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
    alphas <- ElInputalphas()
    c(a,b,N,f,alphas) := ElInput2()
    repeticiones <- seq(10,N,30)
    sapply(repeticiones,confianza, simplify = FALSE) %>%
      bind_rows()
  })
  
  output$text2 <- renderText({
    
    #integral <- round(LaIntegral(), 5)
    quienes <- Intervalos()
    integral <- (quienes$media)[length(quienes$media)]
    
    as.character(integral)
  })
  
  output$graf_fun <- renderPlot({
    alphas <- ElInputalphas()
    c(a,b,N,f) := ElInput2()
    x <- seq(a,b,(b-a)/N)
    y <- f(x)
    df <- data.frame(x,y)
    
    ggplot(df,aes(x,y))+
      geom_line(colour="#990000",size=1.5)+
      geom_area(fill="#06b9C7",alpha=0.3)+
      ylab('f(x)')
  })
  
  output$graf_conf <- renderPlot({
    alphas <- ElInputalphas()
    c(a,b,N,f) := ElInput2()
    ggplot(Intervalos(), aes(x = (Numero), y = media)) + 
      geom_ribbon(aes(ymin = minimo, ymax = maximo), 
                  alpha =0.4, fill = '#06b9C7') + 
      geom_line(color = 'black', size = 0.6) + 
      ylab('Valor de la integral MC') + 
      xlab("Número de simulaciones")+ 
      ggtitle("Intervalos de Confianza")
  })
  
  
  ###########################################################################
  ###########################################################################
  #################                                         #################
  #################             Tarea      4                #################
  #################                                         #################
  ###########################################################################
  ###########################################################################

  
  output$table <- renderDataTable(datatable({
    concre <- read.csv("./Data/concreto.csv")
    concre
  }))
  
  ElInput3 <- reactive({
    nombres <- c(names(concre))
    for(i in 1:length(nombres)){
      if(input$X == nombres[i]) {
        ind <- i
        namei <- nombres[i]
      }
      if(input$Y == nombres[i]) {
        dep <- i
        named <- nombres[i]
      }
    }
  return(list(ind,dep,namei,named))
  })
  
  
  output$disperso <- renderPlot({
    c(ind,dep,namei,named) := ElInput3()
   ggplot(concre, aes(x = concre[,ind], y = concre[,dep])) + 
    geom_point(alpha =0.4, fill = '#08b9C7') + 
    ylab(namei) + 
    xlab(named)+
    ggtitle(eval(namei),eval(named))
  })
   
  output$distribua <- renderPlot({
    alph <- c(seq(-4, 4, length=100))
    dalph <- c(dnorm(alph,0.0001))
    dfal <- data.frame(alph,dalph)
    colnames(dfal) <- c("alpha","distr")
    p1 <- ggplot(dfal,aes(x=alpha,y=distr))+
      geom_line(fill='#08b9c7')+ 
      ylab("P(alpha)") + 
      xlab("alpha")+
      ggtitle("PDF alpha")
    p1
  })
  
  output$distribub <- renderPlot({
    bet <- c(seq(-4, 4, length=100))
    dbet <- c(dnorm(bet,0.0001))
    dfbe <- data.frame(bet,dbet)
    colnames(dfbe) <- c("beta", "distr")
    p2 <- ggplot(dfbe,aes(x=beta,y=distr))+
      geom_line(fill='#08b9c7')+ 
      ylab("P(beta)") + 
      xlab("beta")+
      ggtitle("PDF beta")
    p2
  })
  output$distribut <- renderPlot({
    tau <- c(seq(0, 4, length=1000))
    dtau <- c(dgamma(tau,0.0001))
    dfta <- data.frame(tau,dtau)
    colnames(dfta) <- c("tau", "distr")
    p2 <- ggplot(dfta,aes(x=tau,y=distr))+
      geom_line(fill='#08b9c7')+ 
      ylab("P(tau)") + 
      xlab("tau")+
      ggtitle("PDF tau")
    p2
  })
})