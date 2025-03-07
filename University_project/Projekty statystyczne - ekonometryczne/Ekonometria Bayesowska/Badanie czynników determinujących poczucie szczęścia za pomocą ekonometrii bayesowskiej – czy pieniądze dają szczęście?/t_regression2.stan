data {
  int<lower=0> N;
  int<lower=0> k;
  int<lower=0> l; //no. of column with distr description
  real y[N];
  matrix[k,N] X;
  matrix[k,l] priors;
  //vector[k] beta_prior;
  //vector[k] beta_sd_prior;
}
parameters {
  real<lower=0> h;
  vector[k] beta;
}
model {
  target += gamma_lpdf(h | 1, 1);
  for (ii in 1:k) {
    if (priors[ii,l] == 0) {
      target += normal_lpdf(beta[ii] | priors[ii,1],priors[ii,2]);
    } else if (priors[ii,l]==1) {
      target += gamma_lpdf(beta[ii] | priors[ii,3],priors[ii,4]);
    }
  }
  for (nn in 1:N) {
    target += student_t_lpdf(y[nn] | 8, (X[,nn])'*beta, 1/sqrt(h));
  }
}
