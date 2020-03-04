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
  
  
  // Calculate likelihood
  Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > llt(svar);
  matrix<Type> L_svar = llt.matrixL();
  vector<Type> D = L_svar.diagonal();
  Type halfLogDetS = sum(log(D));
  matrix<Type> inv_L_svar = atomic::matinv(L_svar);
  
  
  matrix<Type> res(nVar, nObs - 1);
  for (int i = 0; i < (nObs - 1); i++) {
    res.col(i) = x.col(i+1) - x.col(i);
  }
  
  matrix<Type> z = inv_L_svar * res;
  
  Type nll = 0;
  
  for (int i = 0; i < nVar; i++) {
    for (int j = 0; j < (nObs - 1); j++) {

      nll -= dnorm(Type(z(i,j)), Type(0.0), Type(1.0), true);

    }
  }
  
  nll += halfLogDetS;
    
  
  ADREPORT(svar);
  ADREPORT(scor);
  ADREPORT(ssd);
  ADREPORT(rho);
  REPORT(scor);
  
  return nll;
  
}

