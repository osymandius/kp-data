#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
Type objective_function<Type>::operator() ()

{

  using namespace density;
  using namespace Eigen;

  Type nll = 0;

  PARAMETER(beta_0);

  DATA_SPARSE_MATRIX(A_age);
  DATA_SPARSE_MATRIX(five_year_A_age);

  DATA_VECTOR(age_span);
  DATA_VECTOR(log_offset)

  DATA_VECTOR(denominator);
  
  ////////

  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);

  PARAMETER_VECTOR(u_age);
  PARAMETER(log_prec_age);
  DATA_SPARSE_MATRIX(R_age);
  DATA_SPARSE_MATRIX(Z_age);
  // PARAMETER(logit_phi_age);

  Type prec_age = exp(log_prec_age);
  nll -= dgamma(prec_age, Type(1), Type(2000), true);

  // Type phi_age(exp(logit_phi_age)/(1+exp(logit_phi_age)));
  // nll -= log(phi_age) +  log(1 - phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(phi_age, Type(0.5), Type(0.5), true);
  // nll += AR1(Type(phi_age))(u_age);

  nll -= Type(-0.5) * (u_age * (R_age * u_age)).sum();
  nll -= dnorm(u_age.sum(), Type(0), Type(0.001) * u_age.size(), 1);

  ///////////////////

  // PARAMETER_VECTOR(u_spatial);
  // PARAMETER(log_prec_spatial);
  // DATA_SPARSE_MATRIX(Z_spatial);

  // Type prec_spatial = exp(log_prec_spatial);

  // nll -= dgamma(prec_spatial, Type(1), Type(2000), true);
  // nll -= Type(-0.5) * (u_spatial * (R_spatial * u_spatial)).sum();
  // nll -= dnorm(u_spatial.sum(), Type(0), Type(0.01) * u_spatial.size(), 1);

  ////////

  // PARAMETER_VECTOR(u_ref);
  // PARAMETER(log_prec_ref);
  // DATA_SPARSE_MATRIX(Z_ref);

  // Type prec_ref = exp(log_prec_ref);

  // nll -= dgamma(prec_ref, Type(1), Type(2000), true);
  // nll -= Type(-0.5) * (u_ref * (R_ref * u_ref)).sum();

  ////

  vector<Type> log_lambda(
                    beta_0 
                    + Z_age * u_age * sqrt(1/prec_age)
    );

  vector<Type> lambda = exp(log_lambda);
  // nll -= dnorm(lambda.sum(), Type(1), Type(0.00001) * lambda.size(), 1);

  // 1/((A_age * exp(log_lambda).inverse()/age_span)

  vector<Type> log_age_count_pred(
                          A_age * exp(log_lambda)
                          + log_offset
                          );

  // vector<Type> aggregated_age = (A_age * lambda)/age_span;
  
  // vector<Type> logit_aggregated_u_age(log(aggregated_u_age/(1-aggregated_u_age)));

  ///////////////////////


  nll -= dpois(denominator, exp(log_age_count_pred), true).sum();

  // vector<Type> age_out(five_year_A_age * log_lambda);

  // REPORT(age_out);
  REPORT(lambda);

  return nll;

}
