concre <- read.csv("./Data/concreto.csv")

paquetines <- c("shiny","shinythemes","dplyr","plotly","ggplot2","markdown","DT","grid","gridExtra")
no_instalados <- paquetines[!(paquetines %in% installed.packages()[,"Package"])]
if(length(no_instalados)) install.packages(no_instalados)
lapply(paquetines, library, character.only = TRUE)
Rcpp::sourceCpp("mhMCMC.cpp")

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
    dalph <- c(dnorm(alph,1))
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
    dbet <- c(dnorm(bet,1))
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
  
  ###########################################################################
  ###########################################################################
  #################                                         #################
  #################             Tarea      5                #################
  #################                                         #################
  ###########################################################################
  ###########################################################################
  
  chain <- reactive({
    c(ind,dep,namei,named) := ElInput3()
      theta0 <- c(1,1,1)
      chain <- mhMCMC(x = concre[,ind], y = concre[,dep], startValue=theta0, iterations=input$sLongitud)
      return(data.frame(alpha=chain[,1], beta=chain[,2], tau=chain[,3]))
  })
  
  
  df <- eventReactive(input$button, {
    c(ind,dep,namei,named) := ElInput3()
      theta0 <- c(1,1,1)
      chain <- mhMCMC(x = concre[,ind], y = concre[,dep], startValue=theta0, iterations=input$sLongitud)
      chain <- data.frame(alpha=chain[,1], beta=chain[,2], tau=chain[,3])
      for (i in 1:input$nCadenas-1){
        aux <- theta0 + round(10*runif(1))
        aux2 <- mhMCMC(x = concre[,ind], y = concre[,dep], startValue=aux, iterations=input$sLongitud)
        aux2 <- data.frame(alpha=aux2[,1], beta=aux2[,2], tau=aux2[,3])
        chain <- cbind(chain, aux2)
      return(chain)
    }
    
  })
  
  output$cadenasMCMC <- DT::renderDataTable(DT::datatable({
    if(is.null(df()))
      return()
    else 
      return(df())
  }))
  
  output$hist_alpha <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alpha_sb = df()[-(1:input$sBurnin),1]
      qplot(alpha_sb, geom = "histogram",binwidth = 0.2,
            main = "Histograma de alpha",
            xlab = "alpha",
            fill=I("blue"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })
  
  output$hist_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[-(1:input$sBurnin),2]
      qplot(beta_sb, geom = "histogram",binwidth = 0.001,
            main = "Histograma de beta",
            xlab = "beta",
            fill=I("red"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })
  
  output$hist_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[-(1:input$sBurnin),3]
      qplot(sigma_sb, geom = "histogram",binwidth = 0.05,
            main = "Histograma de sigma",
            xlab = "sigma",
            fill=I("green"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })

  output$dens_alpha <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alpha_sb = df()[-(1:input$sBurnin),1]
      media <- mean(alpha_sb)
      desv <- sd(alpha_sb)
      alph <- c(seq(-4, 15, length=100))
      dalph <- c(dnorm(alph,1))
      xfit<-seq(min(alph),max(alpha_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfal <- data.frame(alph,dalph)
      colnames(dfal) <- c("alpha","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfal,aes(x=alph,y=dalph),colour="#990000",size=1.5)+
        geom_area(data=dfal,aes(x=alph,y=dalph),fill="#ff00ff",alpha=0.3)+ 
        ylab('P(alpha)') + 
        xlab("alpha")+ 
        ggtitle("Densidad a priori y a posteriori de alpha")
      })
    }
  })
  
  output$dens_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[-(1:input$sBurnin),2]
      media <- mean(beta_sb)
      desv <- sd(beta_sb)
      bet <- c(seq(-4, 15, length=100))
      dbet <- c(dnorm(bet,1))
      xfit<-seq(min(bet),max(beta_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfbe <- data.frame(bet,dbet)
      colnames(dfbe) <- c("beta","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfbe,aes(x=beta,y=distr),colour="#990000",size=1.5)+
        geom_area(data=dfbe,aes(x=beta,y=distr),fill="#ff00ff",alpha=0.3)+ 
        ylab('P(beta)') + 
        xlab("beta")
      })
    }
  })
  
  output$dens_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[-(1:input$sBurnin),3]
      media <- mean(sigma_sb)
      desv <- sd(sigma_sb)
      sigm <- c(seq(0, 4, length=1000))
      dsigm <- c(dgamma(sigm,0.01))
      xfit<-seq(min(sigm),max(sigma_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfsi <- data.frame(sigm,dsigm)
      colnames(dfsi) <- c("sigma","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfsi,aes(x=sigma,y=distr),colour="#990000",size=1.5)+
        geom_area(data=dfsi,aes(x=sigma,y=distr),fill="#ff00ff",alpha=0.3)+ 
        ylab('P(alpha)') + 
        xlab("alpha")+ 
        ggtitle("Densidad a priori y a posteriori de alpha")
      })
    }
  })
  
  output$cadena_alpha <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alpha_sb = df()[,1]
      iteracion <- seq(1,length(alpha_sb),length=length(alpha_sb))
      df <- data.frame(alpha_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=alpha_sb))+
        geom_line()+
        ylab('alpha') + 
        xlab("Iteración")+ 
        ggtitle("Serie de alpha")
      })
    }
  })
  
  output$cadena_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[,2]
      iteracion <- seq(1,length(beta_sb),length=length(beta_sb))
      df <- data.frame(beta_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=beta_sb))+
        geom_line()+
        ylab('beta') + 
        xlab("Iteración")+ 
        ggtitle("Serie de beta")
      })
    }
  })
  
  output$cadena_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[,3]
      iteracion <- seq(1,length(sigma_sb),length=length(sigma_sb))
      df <- data.frame(sigma_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=sigma_sb))+
        geom_line()+
        ylab('sigma') + 
        xlab("Iteración")+ 
        ggtitle("Serie de sigma")
      })
    }
  })
  
})