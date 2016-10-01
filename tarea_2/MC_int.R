integral_MC <- function(f,a,b,N=10000){
  
  al_cero <- function(x){dist(c(x,0))}
  
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

