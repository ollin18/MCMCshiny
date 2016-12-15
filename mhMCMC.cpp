#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double logverosimilitud(NumericVector theta, NumericVector X, NumericVector Y){
  double alpha = theta[0];
  double beta = theta[1];
  double tau = theta[2];
  int n = X.length();
  
  NumericVector yf(n);
  for(int i=0; i < n; i++){
    yf[i] = alpha + beta*X[i];
  }
  
  NumericVector verosimilitud(n);
  double derecha;
  double izquierda;
  
  for (int i=0; i < n; i++){
    derecha = exp(-pow(Y[i]-yf[i], 2.0)*0.5/(tau*tau));
    izquierda = sqrt(2*M_PI)*tau;
    verosimilitud[i] = log(derecha/izquierda);
  }
  
  double suma = sum(verosimilitud);
  return (suma);
}

// [[Rcpp::export]]
double apriori(NumericVector theta){
  double alpha = theta[0];
  double beta = theta[1];
  double tau = theta[2];
  
  double a_ = R::dnorm(alpha, 0, 50, true);
  double b_ = R::dnorm(beta, 0, 50, true);
  double tau_ = R::dgamma(tau, 0.01, 50,true);
  double aux = a_+b_+tau_;
  return(aux);
}

// [[Rcpp::export]]
double posteriori(NumericVector theta, NumericVector X, NumericVector Y){
  double aux1 = logverosimilitud(theta, X, Y);
  double aux2 = apriori(theta);
  aux2 = aux2 + aux1;
  return(aux2);
}

// [[Rcpp::export]]
NumericVector proposal(NumericVector theta){
  double alpha = theta[0];
  double beta = theta[1];
  double tau = theta[2];
  
  double p1 = R::rnorm(alpha,  2.0);
  double p2 = R::rnorm(beta,  2.0);
  double p3 = R::rnorm(tau, 2.0);
  
  NumericVector aux(3);
  aux[0] = p1;
  aux[1] = p2;
  aux[2] = p3;
  return (aux);
}

// [[Rcpp::export]]
NumericMatrix mhMCMC(NumericVector x, NumericVector y, NumericVector startValue, int iterations){
  NumericMatrix chain(iterations+1, 3);
  for(int i=0; i < startValue.length(); i++){
    chain(0,i) = startValue[i];
  }
  
  NumericVector prop(3);
  NumericVector aux(3);
  double probab;
  for(int i=0; i < iterations; i++){
    for(int j=0; j < 3; j++){            //auxiliar, vector parametros
      aux[j] = chain(i, j);
    }
    prop = proposal(aux);
    probab = exp(posteriori(prop, x, y) - posteriori(aux, x, y));
    if(R::runif(0,1) < probab){
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = prop[j];
      }
    }else{
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = chain(i,j);
      }
    }
  }
  return(chain);
}

  
  
  
  
  
  
  
  
  
  









