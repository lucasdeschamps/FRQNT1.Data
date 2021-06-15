data{
  // Number of data points
  int N;
  
  // Calibration parameters
  real A;
  real B;
  real C;
  real D;
  
  //Observed VWC
  real Theta[N];
}
parameters{
  real<lower=0> P[N];
  real<lower=0> sigma;
}
model{
  for(n in 1:N) Theta[n] ~ normal(A + B * P[n] + C * pow(P[n],2) + D * pow(P[n],3), sigma);
  
  sigma ~ lognormal(0,1);
}