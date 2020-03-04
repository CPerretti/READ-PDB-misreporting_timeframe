// Fit a multivariate normal distribution
#include <TMB.hpp>

template <class Type>
Type trans(Type x){
  return Type(2)/(Type(1) + exp(-Type(2) * x)) - Type(1);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  
  DATA_ARRAY(x);
  
  PARAMETER_VECTOR(logSdS);
  PARAMETER(itrans_rho);
  
  int nVar = x.dim[0];
  int nObs = x.dim[1];
  
  matrix<Type> scor(nVar, nVar);
  matrix<Type> svar(nVar, nVar);
  vector<Type> ssd = exp(logSdS);
  Type rho = trans(itrans_rho);
  
  for(int i = 0; i < nVar; ++i){
    scor(i,i) = 1.0;
  }
  
  for(int i = 0; i < nVar; ++i){
    for(int j = 0; j < i; ++j){
      scor(i,j) = pow(rho, abs(Type(i-j)));
      scor(j,i) = scor(i,j);
    }
  } 
  
  int i, j;
  for(i = 0; i < nVar; ++i){
    for(j = 0; j < nVar; ++j){
      svar(i,j) = ssd(i) * ssd(j) * scor(i,j);
    }
  }
    
  using namespace density;
  MVNORM_t<Type> your_dnorm(svar);
  
  Type nll = 0;
  
  for (int i = 1; i < nObs; i++) {
    nll += your_dnorm(x.col(i) - x.col(i-1));
  }
    
  
  ADREPORT(svar);
  ADREPORT(scor);
  ADREPORT(ssd);
  ADREPORT(rho);
  REPORT(scor);
  
  return nll;
  
}

