data {

    int<lower=1> N; // number of observations
    int<lower=2> J;
    vector[J] V_star[N]; // vote choice

    // AREA-LEVEL PREDICTOR
    matrix[N,J] Z; // state- level covariate matrix

    // DATA RANDOM EFFECTS

}

parameters {

    vector[J] beta; // baseline rate of support
    real<lower = 0> beta_sd;

    vector[J] eta_s; // fixed-effects

    // PARAMS RANDOM EFFECTS

}

transformed parameters {

    matrix[N,J] mu_s; 

    vector[J] beta_s = beta * beta_sd;
    
    // TRANSFORMED PARAMS EFFECTS
  
    
    for(j in 1:J) {

    mu_s[1:N,j] = 
    
    beta_s[j] + 
            
    // LINEAR PREDICTOR RANDOM EFFECTS

    Z[1:N, j] * eta_s[j] ;

    }

}

model {
    
  beta ~ std_normal();
  beta_sd ~ std_normal();

  eta_s ~ std_normal();

  // PRIORS RANDOM EFFECTS


  // VOTE CHOICE LIKELIHOOD
  for (i in 1:N) {
    //V[i] ~ categorical_logit(mu_s[i,1:J]');
    target += sum(V_star[i] .* log_softmax(mu_s[i,1:J]'));
  }

}

