data {

    int<lower=1> N; // number of observations
    int<lower=2> J;
    vector[J] V_star[N]; // vote choice

    // AREA-LEVEL PREDICTOR
    matrix[N,J] Z; // state- level covariate matrix

    // DATA RANDOM EFFECTS
  int gend_id[N];
  int<lower = 1> gend_N;

  int ethn_id[N];
  int<lower = 1> ethn_N;

  int educ_id[N];
  int<lower = 1> educ_N;

  int age__id[N];
  int<lower = 1> age__N;

  int inco_id[N];
  int<lower = 1> inco_N;

  int stat_id[N];
  int<lower = 1> stat_N;

  int vote_id[N];
  int<lower = 1> vote_N;

  int gend_AND_ethn_id[N];
  int<lower = 1> gend_AND_ethn_N;

  int gend_AND_educ_id[N];
  int<lower = 1> gend_AND_educ_N;

  int gend_AND_age__id[N];
  int<lower = 1> gend_AND_age__N;

  int gend_AND_inco_id[N];
  int<lower = 1> gend_AND_inco_N;

  int gend_AND_stat_id[N];
  int<lower = 1> gend_AND_stat_N;

  int gend_AND_vote_id[N];
  int<lower = 1> gend_AND_vote_N;

  int ethn_AND_educ_id[N];
  int<lower = 1> ethn_AND_educ_N;

  int ethn_AND_age__id[N];
  int<lower = 1> ethn_AND_age__N;

  int ethn_AND_inco_id[N];
  int<lower = 1> ethn_AND_inco_N;

  int ethn_AND_stat_id[N];
  int<lower = 1> ethn_AND_stat_N;

  int ethn_AND_vote_id[N];
  int<lower = 1> ethn_AND_vote_N;

  int educ_AND_age__id[N];
  int<lower = 1> educ_AND_age__N;

  int educ_AND_inco_id[N];
  int<lower = 1> educ_AND_inco_N;

  int educ_AND_stat_id[N];
  int<lower = 1> educ_AND_stat_N;

  int educ_AND_vote_id[N];
  int<lower = 1> educ_AND_vote_N;

  int age__AND_inco_id[N];
  int<lower = 1> age__AND_inco_N;

  int age__AND_stat_id[N];
  int<lower = 1> age__AND_stat_N;

  int age__AND_vote_id[N];
  int<lower = 1> age__AND_vote_N;

  int inco_AND_stat_id[N];
  int<lower = 1> inco_AND_stat_N;

  int inco_AND_vote_id[N];
  int<lower = 1> inco_AND_vote_N;

  int stat_AND_vote_id[N];
  int<lower = 1> stat_AND_vote_N;

  int gend_AND_ethn_AND_educ_id[N];
  int<lower = 1> gend_AND_ethn_AND_educ_N;

  int gend_AND_ethn_AND_age__id[N];
  int<lower = 1> gend_AND_ethn_AND_age__N;

  int gend_AND_ethn_AND_inco_id[N];
  int<lower = 1> gend_AND_ethn_AND_inco_N;

  int gend_AND_ethn_AND_stat_id[N];
  int<lower = 1> gend_AND_ethn_AND_stat_N;

  int gend_AND_ethn_AND_vote_id[N];
  int<lower = 1> gend_AND_ethn_AND_vote_N;

  int gend_AND_educ_AND_age__id[N];
  int<lower = 1> gend_AND_educ_AND_age__N;

  int gend_AND_educ_AND_inco_id[N];
  int<lower = 1> gend_AND_educ_AND_inco_N;

  int gend_AND_educ_AND_stat_id[N];
  int<lower = 1> gend_AND_educ_AND_stat_N;

  int gend_AND_educ_AND_vote_id[N];
  int<lower = 1> gend_AND_educ_AND_vote_N;

  int gend_AND_age__AND_inco_id[N];
  int<lower = 1> gend_AND_age__AND_inco_N;

  int gend_AND_age__AND_stat_id[N];
  int<lower = 1> gend_AND_age__AND_stat_N;

  int gend_AND_age__AND_vote_id[N];
  int<lower = 1> gend_AND_age__AND_vote_N;

  int gend_AND_inco_AND_stat_id[N];
  int<lower = 1> gend_AND_inco_AND_stat_N;

  int gend_AND_inco_AND_vote_id[N];
  int<lower = 1> gend_AND_inco_AND_vote_N;

  int gend_AND_stat_AND_vote_id[N];
  int<lower = 1> gend_AND_stat_AND_vote_N;

  int ethn_AND_educ_AND_age__id[N];
  int<lower = 1> ethn_AND_educ_AND_age__N;

  int ethn_AND_educ_AND_inco_id[N];
  int<lower = 1> ethn_AND_educ_AND_inco_N;

  int ethn_AND_educ_AND_stat_id[N];
  int<lower = 1> ethn_AND_educ_AND_stat_N;

  int ethn_AND_educ_AND_vote_id[N];
  int<lower = 1> ethn_AND_educ_AND_vote_N;

  int ethn_AND_age__AND_inco_id[N];
  int<lower = 1> ethn_AND_age__AND_inco_N;

  int ethn_AND_age__AND_stat_id[N];
  int<lower = 1> ethn_AND_age__AND_stat_N;

  int ethn_AND_age__AND_vote_id[N];
  int<lower = 1> ethn_AND_age__AND_vote_N;

  int ethn_AND_inco_AND_stat_id[N];
  int<lower = 1> ethn_AND_inco_AND_stat_N;

  int ethn_AND_inco_AND_vote_id[N];
  int<lower = 1> ethn_AND_inco_AND_vote_N;

  int ethn_AND_stat_AND_vote_id[N];
  int<lower = 1> ethn_AND_stat_AND_vote_N;

  int educ_AND_age__AND_inco_id[N];
  int<lower = 1> educ_AND_age__AND_inco_N;

  int educ_AND_age__AND_stat_id[N];
  int<lower = 1> educ_AND_age__AND_stat_N;

  int educ_AND_age__AND_vote_id[N];
  int<lower = 1> educ_AND_age__AND_vote_N;

  int educ_AND_inco_AND_stat_id[N];
  int<lower = 1> educ_AND_inco_AND_stat_N;

  int educ_AND_inco_AND_vote_id[N];
  int<lower = 1> educ_AND_inco_AND_vote_N;

  int educ_AND_stat_AND_vote_id[N];
  int<lower = 1> educ_AND_stat_AND_vote_N;

  int age__AND_inco_AND_stat_id[N];
  int<lower = 1> age__AND_inco_AND_stat_N;

  int age__AND_inco_AND_vote_id[N];
  int<lower = 1> age__AND_inco_AND_vote_N;

  int age__AND_stat_AND_vote_id[N];
  int<lower = 1> age__AND_stat_AND_vote_N;

  int inco_AND_stat_AND_vote_id[N];
  int<lower = 1> inco_AND_stat_AND_vote_N;



}

parameters {

    vector[J] beta; // baseline rate of support
    real<lower = 0> beta_sd;

    vector[J] eta_s; // fixed-effects

    // PARAMS RANDOM EFFECTS
  matrix[gend_N,J] xi_gend;
  real<lower=0> xi_gend_sd;

  matrix[ethn_N,J] xi_ethn;
  real<lower=0> xi_ethn_sd;

  matrix[educ_N,J] xi_educ;
  real<lower=0> xi_educ_sd;

  matrix[age__N,J] xi_age_;
  real<lower=0> xi_age__sd;

  matrix[inco_N,J] xi_inco;
  real<lower=0> xi_inco_sd;

  matrix[stat_N,J] xi_stat;
  real<lower=0> xi_stat_sd;

  matrix[vote_N,J] xi_vote;
  real<lower=0> xi_vote_sd;

  matrix[gend_AND_ethn_N,J] xi_gend_AND_ethn;
  real<lower=0> xi_gend_AND_ethn_sd;

  matrix[gend_AND_educ_N,J] xi_gend_AND_educ;
  real<lower=0> xi_gend_AND_educ_sd;

  matrix[gend_AND_age__N,J] xi_gend_AND_age_;
  real<lower=0> xi_gend_AND_age__sd;

  matrix[gend_AND_inco_N,J] xi_gend_AND_inco;
  real<lower=0> xi_gend_AND_inco_sd;

  matrix[gend_AND_stat_N,J] xi_gend_AND_stat;
  real<lower=0> xi_gend_AND_stat_sd;

  matrix[gend_AND_vote_N,J] xi_gend_AND_vote;
  real<lower=0> xi_gend_AND_vote_sd;

  matrix[ethn_AND_educ_N,J] xi_ethn_AND_educ;
  real<lower=0> xi_ethn_AND_educ_sd;

  matrix[ethn_AND_age__N,J] xi_ethn_AND_age_;
  real<lower=0> xi_ethn_AND_age__sd;

  matrix[ethn_AND_inco_N,J] xi_ethn_AND_inco;
  real<lower=0> xi_ethn_AND_inco_sd;

  matrix[ethn_AND_stat_N,J] xi_ethn_AND_stat;
  real<lower=0> xi_ethn_AND_stat_sd;

  matrix[ethn_AND_vote_N,J] xi_ethn_AND_vote;
  real<lower=0> xi_ethn_AND_vote_sd;

  matrix[educ_AND_age__N,J] xi_educ_AND_age_;
  real<lower=0> xi_educ_AND_age__sd;

  matrix[educ_AND_inco_N,J] xi_educ_AND_inco;
  real<lower=0> xi_educ_AND_inco_sd;

  matrix[educ_AND_stat_N,J] xi_educ_AND_stat;
  real<lower=0> xi_educ_AND_stat_sd;

  matrix[educ_AND_vote_N,J] xi_educ_AND_vote;
  real<lower=0> xi_educ_AND_vote_sd;

  matrix[age__AND_inco_N,J] xi_age__AND_inco;
  real<lower=0> xi_age__AND_inco_sd;

  matrix[age__AND_stat_N,J] xi_age__AND_stat;
  real<lower=0> xi_age__AND_stat_sd;

  matrix[age__AND_vote_N,J] xi_age__AND_vote;
  real<lower=0> xi_age__AND_vote_sd;

  matrix[inco_AND_stat_N,J] xi_inco_AND_stat;
  real<lower=0> xi_inco_AND_stat_sd;

  matrix[inco_AND_vote_N,J] xi_inco_AND_vote;
  real<lower=0> xi_inco_AND_vote_sd;

  matrix[stat_AND_vote_N,J] xi_stat_AND_vote;
  real<lower=0> xi_stat_AND_vote_sd;

  matrix[gend_AND_ethn_AND_educ_N,J] xi_gend_AND_ethn_AND_educ;
  real<lower=0> xi_gend_AND_ethn_AND_educ_sd;

  matrix[gend_AND_ethn_AND_age__N,J] xi_gend_AND_ethn_AND_age_;
  real<lower=0> xi_gend_AND_ethn_AND_age__sd;

  matrix[gend_AND_ethn_AND_inco_N,J] xi_gend_AND_ethn_AND_inco;
  real<lower=0> xi_gend_AND_ethn_AND_inco_sd;

  matrix[gend_AND_ethn_AND_stat_N,J] xi_gend_AND_ethn_AND_stat;
  real<lower=0> xi_gend_AND_ethn_AND_stat_sd;

  matrix[gend_AND_ethn_AND_vote_N,J] xi_gend_AND_ethn_AND_vote;
  real<lower=0> xi_gend_AND_ethn_AND_vote_sd;

  matrix[gend_AND_educ_AND_age__N,J] xi_gend_AND_educ_AND_age_;
  real<lower=0> xi_gend_AND_educ_AND_age__sd;

  matrix[gend_AND_educ_AND_inco_N,J] xi_gend_AND_educ_AND_inco;
  real<lower=0> xi_gend_AND_educ_AND_inco_sd;

  matrix[gend_AND_educ_AND_stat_N,J] xi_gend_AND_educ_AND_stat;
  real<lower=0> xi_gend_AND_educ_AND_stat_sd;

  matrix[gend_AND_educ_AND_vote_N,J] xi_gend_AND_educ_AND_vote;
  real<lower=0> xi_gend_AND_educ_AND_vote_sd;

  matrix[gend_AND_age__AND_inco_N,J] xi_gend_AND_age__AND_inco;
  real<lower=0> xi_gend_AND_age__AND_inco_sd;

  matrix[gend_AND_age__AND_stat_N,J] xi_gend_AND_age__AND_stat;
  real<lower=0> xi_gend_AND_age__AND_stat_sd;

  matrix[gend_AND_age__AND_vote_N,J] xi_gend_AND_age__AND_vote;
  real<lower=0> xi_gend_AND_age__AND_vote_sd;

  matrix[gend_AND_inco_AND_stat_N,J] xi_gend_AND_inco_AND_stat;
  real<lower=0> xi_gend_AND_inco_AND_stat_sd;

  matrix[gend_AND_inco_AND_vote_N,J] xi_gend_AND_inco_AND_vote;
  real<lower=0> xi_gend_AND_inco_AND_vote_sd;

  matrix[gend_AND_stat_AND_vote_N,J] xi_gend_AND_stat_AND_vote;
  real<lower=0> xi_gend_AND_stat_AND_vote_sd;

  matrix[ethn_AND_educ_AND_age__N,J] xi_ethn_AND_educ_AND_age_;
  real<lower=0> xi_ethn_AND_educ_AND_age__sd;

  matrix[ethn_AND_educ_AND_inco_N,J] xi_ethn_AND_educ_AND_inco;
  real<lower=0> xi_ethn_AND_educ_AND_inco_sd;

  matrix[ethn_AND_educ_AND_stat_N,J] xi_ethn_AND_educ_AND_stat;
  real<lower=0> xi_ethn_AND_educ_AND_stat_sd;

  matrix[ethn_AND_educ_AND_vote_N,J] xi_ethn_AND_educ_AND_vote;
  real<lower=0> xi_ethn_AND_educ_AND_vote_sd;

  matrix[ethn_AND_age__AND_inco_N,J] xi_ethn_AND_age__AND_inco;
  real<lower=0> xi_ethn_AND_age__AND_inco_sd;

  matrix[ethn_AND_age__AND_stat_N,J] xi_ethn_AND_age__AND_stat;
  real<lower=0> xi_ethn_AND_age__AND_stat_sd;

  matrix[ethn_AND_age__AND_vote_N,J] xi_ethn_AND_age__AND_vote;
  real<lower=0> xi_ethn_AND_age__AND_vote_sd;

  matrix[ethn_AND_inco_AND_stat_N,J] xi_ethn_AND_inco_AND_stat;
  real<lower=0> xi_ethn_AND_inco_AND_stat_sd;

  matrix[ethn_AND_inco_AND_vote_N,J] xi_ethn_AND_inco_AND_vote;
  real<lower=0> xi_ethn_AND_inco_AND_vote_sd;

  matrix[ethn_AND_stat_AND_vote_N,J] xi_ethn_AND_stat_AND_vote;
  real<lower=0> xi_ethn_AND_stat_AND_vote_sd;

  matrix[educ_AND_age__AND_inco_N,J] xi_educ_AND_age__AND_inco;
  real<lower=0> xi_educ_AND_age__AND_inco_sd;

  matrix[educ_AND_age__AND_stat_N,J] xi_educ_AND_age__AND_stat;
  real<lower=0> xi_educ_AND_age__AND_stat_sd;

  matrix[educ_AND_age__AND_vote_N,J] xi_educ_AND_age__AND_vote;
  real<lower=0> xi_educ_AND_age__AND_vote_sd;

  matrix[educ_AND_inco_AND_stat_N,J] xi_educ_AND_inco_AND_stat;
  real<lower=0> xi_educ_AND_inco_AND_stat_sd;

  matrix[educ_AND_inco_AND_vote_N,J] xi_educ_AND_inco_AND_vote;
  real<lower=0> xi_educ_AND_inco_AND_vote_sd;

  matrix[educ_AND_stat_AND_vote_N,J] xi_educ_AND_stat_AND_vote;
  real<lower=0> xi_educ_AND_stat_AND_vote_sd;

  matrix[age__AND_inco_AND_stat_N,J] xi_age__AND_inco_AND_stat;
  real<lower=0> xi_age__AND_inco_AND_stat_sd;

  matrix[age__AND_inco_AND_vote_N,J] xi_age__AND_inco_AND_vote;
  real<lower=0> xi_age__AND_inco_AND_vote_sd;

  matrix[age__AND_stat_AND_vote_N,J] xi_age__AND_stat_AND_vote;
  real<lower=0> xi_age__AND_stat_AND_vote_sd;

  matrix[inco_AND_stat_AND_vote_N,J] xi_inco_AND_stat_AND_vote;
  real<lower=0> xi_inco_AND_stat_AND_vote_sd;



}

transformed parameters {

    matrix[N,J] mu_s; 

    vector[J] beta_s = beta * beta_sd;
    
    // TRANSFORMED PARAMS EFFECTS
  matrix[gend_N,J] xi_gend_s = xi_gend*xi_gend_sd;

  matrix[ethn_N,J] xi_ethn_s = xi_ethn*xi_ethn_sd;

  matrix[educ_N,J] xi_educ_s = xi_educ*xi_educ_sd;

  matrix[age__N,J] xi_age__s = xi_age_*xi_age__sd;

  matrix[inco_N,J] xi_inco_s = xi_inco*xi_inco_sd;

  matrix[stat_N,J] xi_stat_s = xi_stat*xi_stat_sd;

  matrix[vote_N,J] xi_vote_s = xi_vote*xi_vote_sd;

  matrix[gend_AND_ethn_N,J] xi_gend_AND_ethn_s = xi_gend_AND_ethn*xi_gend_AND_ethn_sd;

  matrix[gend_AND_educ_N,J] xi_gend_AND_educ_s = xi_gend_AND_educ*xi_gend_AND_educ_sd;

  matrix[gend_AND_age__N,J] xi_gend_AND_age__s = xi_gend_AND_age_*xi_gend_AND_age__sd;

  matrix[gend_AND_inco_N,J] xi_gend_AND_inco_s = xi_gend_AND_inco*xi_gend_AND_inco_sd;

  matrix[gend_AND_stat_N,J] xi_gend_AND_stat_s = xi_gend_AND_stat*xi_gend_AND_stat_sd;

  matrix[gend_AND_vote_N,J] xi_gend_AND_vote_s = xi_gend_AND_vote*xi_gend_AND_vote_sd;

  matrix[ethn_AND_educ_N,J] xi_ethn_AND_educ_s = xi_ethn_AND_educ*xi_ethn_AND_educ_sd;

  matrix[ethn_AND_age__N,J] xi_ethn_AND_age__s = xi_ethn_AND_age_*xi_ethn_AND_age__sd;

  matrix[ethn_AND_inco_N,J] xi_ethn_AND_inco_s = xi_ethn_AND_inco*xi_ethn_AND_inco_sd;

  matrix[ethn_AND_stat_N,J] xi_ethn_AND_stat_s = xi_ethn_AND_stat*xi_ethn_AND_stat_sd;

  matrix[ethn_AND_vote_N,J] xi_ethn_AND_vote_s = xi_ethn_AND_vote*xi_ethn_AND_vote_sd;

  matrix[educ_AND_age__N,J] xi_educ_AND_age__s = xi_educ_AND_age_*xi_educ_AND_age__sd;

  matrix[educ_AND_inco_N,J] xi_educ_AND_inco_s = xi_educ_AND_inco*xi_educ_AND_inco_sd;

  matrix[educ_AND_stat_N,J] xi_educ_AND_stat_s = xi_educ_AND_stat*xi_educ_AND_stat_sd;

  matrix[educ_AND_vote_N,J] xi_educ_AND_vote_s = xi_educ_AND_vote*xi_educ_AND_vote_sd;

  matrix[age__AND_inco_N,J] xi_age__AND_inco_s = xi_age__AND_inco*xi_age__AND_inco_sd;

  matrix[age__AND_stat_N,J] xi_age__AND_stat_s = xi_age__AND_stat*xi_age__AND_stat_sd;

  matrix[age__AND_vote_N,J] xi_age__AND_vote_s = xi_age__AND_vote*xi_age__AND_vote_sd;

  matrix[inco_AND_stat_N,J] xi_inco_AND_stat_s = xi_inco_AND_stat*xi_inco_AND_stat_sd;

  matrix[inco_AND_vote_N,J] xi_inco_AND_vote_s = xi_inco_AND_vote*xi_inco_AND_vote_sd;

  matrix[stat_AND_vote_N,J] xi_stat_AND_vote_s = xi_stat_AND_vote*xi_stat_AND_vote_sd;

  matrix[gend_AND_ethn_AND_educ_N,J] xi_gend_AND_ethn_AND_educ_s = xi_gend_AND_ethn_AND_educ*xi_gend_AND_ethn_AND_educ_sd;

  matrix[gend_AND_ethn_AND_age__N,J] xi_gend_AND_ethn_AND_age__s = xi_gend_AND_ethn_AND_age_*xi_gend_AND_ethn_AND_age__sd;

  matrix[gend_AND_ethn_AND_inco_N,J] xi_gend_AND_ethn_AND_inco_s = xi_gend_AND_ethn_AND_inco*xi_gend_AND_ethn_AND_inco_sd;

  matrix[gend_AND_ethn_AND_stat_N,J] xi_gend_AND_ethn_AND_stat_s = xi_gend_AND_ethn_AND_stat*xi_gend_AND_ethn_AND_stat_sd;

  matrix[gend_AND_ethn_AND_vote_N,J] xi_gend_AND_ethn_AND_vote_s = xi_gend_AND_ethn_AND_vote*xi_gend_AND_ethn_AND_vote_sd;

  matrix[gend_AND_educ_AND_age__N,J] xi_gend_AND_educ_AND_age__s = xi_gend_AND_educ_AND_age_*xi_gend_AND_educ_AND_age__sd;

  matrix[gend_AND_educ_AND_inco_N,J] xi_gend_AND_educ_AND_inco_s = xi_gend_AND_educ_AND_inco*xi_gend_AND_educ_AND_inco_sd;

  matrix[gend_AND_educ_AND_stat_N,J] xi_gend_AND_educ_AND_stat_s = xi_gend_AND_educ_AND_stat*xi_gend_AND_educ_AND_stat_sd;

  matrix[gend_AND_educ_AND_vote_N,J] xi_gend_AND_educ_AND_vote_s = xi_gend_AND_educ_AND_vote*xi_gend_AND_educ_AND_vote_sd;

  matrix[gend_AND_age__AND_inco_N,J] xi_gend_AND_age__AND_inco_s = xi_gend_AND_age__AND_inco*xi_gend_AND_age__AND_inco_sd;

  matrix[gend_AND_age__AND_stat_N,J] xi_gend_AND_age__AND_stat_s = xi_gend_AND_age__AND_stat*xi_gend_AND_age__AND_stat_sd;

  matrix[gend_AND_age__AND_vote_N,J] xi_gend_AND_age__AND_vote_s = xi_gend_AND_age__AND_vote*xi_gend_AND_age__AND_vote_sd;

  matrix[gend_AND_inco_AND_stat_N,J] xi_gend_AND_inco_AND_stat_s = xi_gend_AND_inco_AND_stat*xi_gend_AND_inco_AND_stat_sd;

  matrix[gend_AND_inco_AND_vote_N,J] xi_gend_AND_inco_AND_vote_s = xi_gend_AND_inco_AND_vote*xi_gend_AND_inco_AND_vote_sd;

  matrix[gend_AND_stat_AND_vote_N,J] xi_gend_AND_stat_AND_vote_s = xi_gend_AND_stat_AND_vote*xi_gend_AND_stat_AND_vote_sd;

  matrix[ethn_AND_educ_AND_age__N,J] xi_ethn_AND_educ_AND_age__s = xi_ethn_AND_educ_AND_age_*xi_ethn_AND_educ_AND_age__sd;

  matrix[ethn_AND_educ_AND_inco_N,J] xi_ethn_AND_educ_AND_inco_s = xi_ethn_AND_educ_AND_inco*xi_ethn_AND_educ_AND_inco_sd;

  matrix[ethn_AND_educ_AND_stat_N,J] xi_ethn_AND_educ_AND_stat_s = xi_ethn_AND_educ_AND_stat*xi_ethn_AND_educ_AND_stat_sd;

  matrix[ethn_AND_educ_AND_vote_N,J] xi_ethn_AND_educ_AND_vote_s = xi_ethn_AND_educ_AND_vote*xi_ethn_AND_educ_AND_vote_sd;

  matrix[ethn_AND_age__AND_inco_N,J] xi_ethn_AND_age__AND_inco_s = xi_ethn_AND_age__AND_inco*xi_ethn_AND_age__AND_inco_sd;

  matrix[ethn_AND_age__AND_stat_N,J] xi_ethn_AND_age__AND_stat_s = xi_ethn_AND_age__AND_stat*xi_ethn_AND_age__AND_stat_sd;

  matrix[ethn_AND_age__AND_vote_N,J] xi_ethn_AND_age__AND_vote_s = xi_ethn_AND_age__AND_vote*xi_ethn_AND_age__AND_vote_sd;

  matrix[ethn_AND_inco_AND_stat_N,J] xi_ethn_AND_inco_AND_stat_s = xi_ethn_AND_inco_AND_stat*xi_ethn_AND_inco_AND_stat_sd;

  matrix[ethn_AND_inco_AND_vote_N,J] xi_ethn_AND_inco_AND_vote_s = xi_ethn_AND_inco_AND_vote*xi_ethn_AND_inco_AND_vote_sd;

  matrix[ethn_AND_stat_AND_vote_N,J] xi_ethn_AND_stat_AND_vote_s = xi_ethn_AND_stat_AND_vote*xi_ethn_AND_stat_AND_vote_sd;

  matrix[educ_AND_age__AND_inco_N,J] xi_educ_AND_age__AND_inco_s = xi_educ_AND_age__AND_inco*xi_educ_AND_age__AND_inco_sd;

  matrix[educ_AND_age__AND_stat_N,J] xi_educ_AND_age__AND_stat_s = xi_educ_AND_age__AND_stat*xi_educ_AND_age__AND_stat_sd;

  matrix[educ_AND_age__AND_vote_N,J] xi_educ_AND_age__AND_vote_s = xi_educ_AND_age__AND_vote*xi_educ_AND_age__AND_vote_sd;

  matrix[educ_AND_inco_AND_stat_N,J] xi_educ_AND_inco_AND_stat_s = xi_educ_AND_inco_AND_stat*xi_educ_AND_inco_AND_stat_sd;

  matrix[educ_AND_inco_AND_vote_N,J] xi_educ_AND_inco_AND_vote_s = xi_educ_AND_inco_AND_vote*xi_educ_AND_inco_AND_vote_sd;

  matrix[educ_AND_stat_AND_vote_N,J] xi_educ_AND_stat_AND_vote_s = xi_educ_AND_stat_AND_vote*xi_educ_AND_stat_AND_vote_sd;

  matrix[age__AND_inco_AND_stat_N,J] xi_age__AND_inco_AND_stat_s = xi_age__AND_inco_AND_stat*xi_age__AND_inco_AND_stat_sd;

  matrix[age__AND_inco_AND_vote_N,J] xi_age__AND_inco_AND_vote_s = xi_age__AND_inco_AND_vote*xi_age__AND_inco_AND_vote_sd;

  matrix[age__AND_stat_AND_vote_N,J] xi_age__AND_stat_AND_vote_s = xi_age__AND_stat_AND_vote*xi_age__AND_stat_AND_vote_sd;

  matrix[inco_AND_stat_AND_vote_N,J] xi_inco_AND_stat_AND_vote_s = xi_inco_AND_stat_AND_vote*xi_inco_AND_stat_AND_vote_sd;


  
    
    for(j in 1:J) {

    mu_s[1:N,j] = 
    
    beta_s[j] + 
            
    // LINEAR PREDICTOR RANDOM EFFECTS
      xi_gend_s[gend_id,j] +

      xi_ethn_s[ethn_id,j] +

      xi_educ_s[educ_id,j] +

      xi_age__s[age__id,j] +

      xi_inco_s[inco_id,j] +

      xi_stat_s[stat_id,j] +

      xi_vote_s[vote_id,j] +

      xi_gend_AND_ethn_s[gend_AND_ethn_id,j] +

      xi_gend_AND_educ_s[gend_AND_educ_id,j] +

      xi_gend_AND_age__s[gend_AND_age__id,j] +

      xi_gend_AND_inco_s[gend_AND_inco_id,j] +

      xi_gend_AND_stat_s[gend_AND_stat_id,j] +

      xi_gend_AND_vote_s[gend_AND_vote_id,j] +

      xi_ethn_AND_educ_s[ethn_AND_educ_id,j] +

      xi_ethn_AND_age__s[ethn_AND_age__id,j] +

      xi_ethn_AND_inco_s[ethn_AND_inco_id,j] +

      xi_ethn_AND_stat_s[ethn_AND_stat_id,j] +

      xi_ethn_AND_vote_s[ethn_AND_vote_id,j] +

      xi_educ_AND_age__s[educ_AND_age__id,j] +

      xi_educ_AND_inco_s[educ_AND_inco_id,j] +

      xi_educ_AND_stat_s[educ_AND_stat_id,j] +

      xi_educ_AND_vote_s[educ_AND_vote_id,j] +

      xi_age__AND_inco_s[age__AND_inco_id,j] +

      xi_age__AND_stat_s[age__AND_stat_id,j] +

      xi_age__AND_vote_s[age__AND_vote_id,j] +

      xi_inco_AND_stat_s[inco_AND_stat_id,j] +

      xi_inco_AND_vote_s[inco_AND_vote_id,j] +

      xi_stat_AND_vote_s[stat_AND_vote_id,j] +

      xi_gend_AND_ethn_AND_educ_s[gend_AND_ethn_AND_educ_id,j] +

      xi_gend_AND_ethn_AND_age__s[gend_AND_ethn_AND_age__id,j] +

      xi_gend_AND_ethn_AND_inco_s[gend_AND_ethn_AND_inco_id,j] +

      xi_gend_AND_ethn_AND_stat_s[gend_AND_ethn_AND_stat_id,j] +

      xi_gend_AND_ethn_AND_vote_s[gend_AND_ethn_AND_vote_id,j] +

      xi_gend_AND_educ_AND_age__s[gend_AND_educ_AND_age__id,j] +

      xi_gend_AND_educ_AND_inco_s[gend_AND_educ_AND_inco_id,j] +

      xi_gend_AND_educ_AND_stat_s[gend_AND_educ_AND_stat_id,j] +

      xi_gend_AND_educ_AND_vote_s[gend_AND_educ_AND_vote_id,j] +

      xi_gend_AND_age__AND_inco_s[gend_AND_age__AND_inco_id,j] +

      xi_gend_AND_age__AND_stat_s[gend_AND_age__AND_stat_id,j] +

      xi_gend_AND_age__AND_vote_s[gend_AND_age__AND_vote_id,j] +

      xi_gend_AND_inco_AND_stat_s[gend_AND_inco_AND_stat_id,j] +

      xi_gend_AND_inco_AND_vote_s[gend_AND_inco_AND_vote_id,j] +

      xi_gend_AND_stat_AND_vote_s[gend_AND_stat_AND_vote_id,j] +

      xi_ethn_AND_educ_AND_age__s[ethn_AND_educ_AND_age__id,j] +

      xi_ethn_AND_educ_AND_inco_s[ethn_AND_educ_AND_inco_id,j] +

      xi_ethn_AND_educ_AND_stat_s[ethn_AND_educ_AND_stat_id,j] +

      xi_ethn_AND_educ_AND_vote_s[ethn_AND_educ_AND_vote_id,j] +

      xi_ethn_AND_age__AND_inco_s[ethn_AND_age__AND_inco_id,j] +

      xi_ethn_AND_age__AND_stat_s[ethn_AND_age__AND_stat_id,j] +

      xi_ethn_AND_age__AND_vote_s[ethn_AND_age__AND_vote_id,j] +

      xi_ethn_AND_inco_AND_stat_s[ethn_AND_inco_AND_stat_id,j] +

      xi_ethn_AND_inco_AND_vote_s[ethn_AND_inco_AND_vote_id,j] +

      xi_ethn_AND_stat_AND_vote_s[ethn_AND_stat_AND_vote_id,j] +

      xi_educ_AND_age__AND_inco_s[educ_AND_age__AND_inco_id,j] +

      xi_educ_AND_age__AND_stat_s[educ_AND_age__AND_stat_id,j] +

      xi_educ_AND_age__AND_vote_s[educ_AND_age__AND_vote_id,j] +

      xi_educ_AND_inco_AND_stat_s[educ_AND_inco_AND_stat_id,j] +

      xi_educ_AND_inco_AND_vote_s[educ_AND_inco_AND_vote_id,j] +

      xi_educ_AND_stat_AND_vote_s[educ_AND_stat_AND_vote_id,j] +

      xi_age__AND_inco_AND_stat_s[age__AND_inco_AND_stat_id,j] +

      xi_age__AND_inco_AND_vote_s[age__AND_inco_AND_vote_id,j] +

      xi_age__AND_stat_AND_vote_s[age__AND_stat_AND_vote_id,j] +

      xi_inco_AND_stat_AND_vote_s[inco_AND_stat_AND_vote_id,j] +



    Z[1:N, j] * eta_s[j] ;

    }

}

model {
    
  beta ~ std_normal();
  beta_sd ~ std_normal();

  eta_s ~ std_normal();

  // PRIORS RANDOM EFFECTS
  to_vector(xi_gend) ~ std_normal();
  xi_gend_sd ~ std_normal();

  to_vector(xi_ethn) ~ std_normal();
  xi_ethn_sd ~ std_normal();

  to_vector(xi_educ) ~ std_normal();
  xi_educ_sd ~ std_normal();

  to_vector(xi_age_) ~ std_normal();
  xi_age__sd ~ std_normal();

  to_vector(xi_inco) ~ std_normal();
  xi_inco_sd ~ std_normal();

  to_vector(xi_stat) ~ std_normal();
  xi_stat_sd ~ std_normal();

  to_vector(xi_vote) ~ std_normal();
  xi_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_ethn) ~ std_normal();
  xi_gend_AND_ethn_sd ~ std_normal();

  to_vector(xi_gend_AND_educ) ~ std_normal();
  xi_gend_AND_educ_sd ~ std_normal();

  to_vector(xi_gend_AND_age_) ~ std_normal();
  xi_gend_AND_age__sd ~ std_normal();

  to_vector(xi_gend_AND_inco) ~ std_normal();
  xi_gend_AND_inco_sd ~ std_normal();

  to_vector(xi_gend_AND_stat) ~ std_normal();
  xi_gend_AND_stat_sd ~ std_normal();

  to_vector(xi_gend_AND_vote) ~ std_normal();
  xi_gend_AND_vote_sd ~ std_normal();

  to_vector(xi_ethn_AND_educ) ~ std_normal();
  xi_ethn_AND_educ_sd ~ std_normal();

  to_vector(xi_ethn_AND_age_) ~ std_normal();
  xi_ethn_AND_age__sd ~ std_normal();

  to_vector(xi_ethn_AND_inco) ~ std_normal();
  xi_ethn_AND_inco_sd ~ std_normal();

  to_vector(xi_ethn_AND_stat) ~ std_normal();
  xi_ethn_AND_stat_sd ~ std_normal();

  to_vector(xi_ethn_AND_vote) ~ std_normal();
  xi_ethn_AND_vote_sd ~ std_normal();

  to_vector(xi_educ_AND_age_) ~ std_normal();
  xi_educ_AND_age__sd ~ std_normal();

  to_vector(xi_educ_AND_inco) ~ std_normal();
  xi_educ_AND_inco_sd ~ std_normal();

  to_vector(xi_educ_AND_stat) ~ std_normal();
  xi_educ_AND_stat_sd ~ std_normal();

  to_vector(xi_educ_AND_vote) ~ std_normal();
  xi_educ_AND_vote_sd ~ std_normal();

  to_vector(xi_age__AND_inco) ~ std_normal();
  xi_age__AND_inco_sd ~ std_normal();

  to_vector(xi_age__AND_stat) ~ std_normal();
  xi_age__AND_stat_sd ~ std_normal();

  to_vector(xi_age__AND_vote) ~ std_normal();
  xi_age__AND_vote_sd ~ std_normal();

  to_vector(xi_inco_AND_stat) ~ std_normal();
  xi_inco_AND_stat_sd ~ std_normal();

  to_vector(xi_inco_AND_vote) ~ std_normal();
  xi_inco_AND_vote_sd ~ std_normal();

  to_vector(xi_stat_AND_vote) ~ std_normal();
  xi_stat_AND_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_ethn_AND_educ) ~ std_normal();
  xi_gend_AND_ethn_AND_educ_sd ~ std_normal();

  to_vector(xi_gend_AND_ethn_AND_age_) ~ std_normal();
  xi_gend_AND_ethn_AND_age__sd ~ std_normal();

  to_vector(xi_gend_AND_ethn_AND_inco) ~ std_normal();
  xi_gend_AND_ethn_AND_inco_sd ~ std_normal();

  to_vector(xi_gend_AND_ethn_AND_stat) ~ std_normal();
  xi_gend_AND_ethn_AND_stat_sd ~ std_normal();

  to_vector(xi_gend_AND_ethn_AND_vote) ~ std_normal();
  xi_gend_AND_ethn_AND_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_educ_AND_age_) ~ std_normal();
  xi_gend_AND_educ_AND_age__sd ~ std_normal();

  to_vector(xi_gend_AND_educ_AND_inco) ~ std_normal();
  xi_gend_AND_educ_AND_inco_sd ~ std_normal();

  to_vector(xi_gend_AND_educ_AND_stat) ~ std_normal();
  xi_gend_AND_educ_AND_stat_sd ~ std_normal();

  to_vector(xi_gend_AND_educ_AND_vote) ~ std_normal();
  xi_gend_AND_educ_AND_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_age__AND_inco) ~ std_normal();
  xi_gend_AND_age__AND_inco_sd ~ std_normal();

  to_vector(xi_gend_AND_age__AND_stat) ~ std_normal();
  xi_gend_AND_age__AND_stat_sd ~ std_normal();

  to_vector(xi_gend_AND_age__AND_vote) ~ std_normal();
  xi_gend_AND_age__AND_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_inco_AND_stat) ~ std_normal();
  xi_gend_AND_inco_AND_stat_sd ~ std_normal();

  to_vector(xi_gend_AND_inco_AND_vote) ~ std_normal();
  xi_gend_AND_inco_AND_vote_sd ~ std_normal();

  to_vector(xi_gend_AND_stat_AND_vote) ~ std_normal();
  xi_gend_AND_stat_AND_vote_sd ~ std_normal();

  to_vector(xi_ethn_AND_educ_AND_age_) ~ std_normal();
  xi_ethn_AND_educ_AND_age__sd ~ std_normal();

  to_vector(xi_ethn_AND_educ_AND_inco) ~ std_normal();
  xi_ethn_AND_educ_AND_inco_sd ~ std_normal();

  to_vector(xi_ethn_AND_educ_AND_stat) ~ std_normal();
  xi_ethn_AND_educ_AND_stat_sd ~ std_normal();

  to_vector(xi_ethn_AND_educ_AND_vote) ~ std_normal();
  xi_ethn_AND_educ_AND_vote_sd ~ std_normal();

  to_vector(xi_ethn_AND_age__AND_inco) ~ std_normal();
  xi_ethn_AND_age__AND_inco_sd ~ std_normal();

  to_vector(xi_ethn_AND_age__AND_stat) ~ std_normal();
  xi_ethn_AND_age__AND_stat_sd ~ std_normal();

  to_vector(xi_ethn_AND_age__AND_vote) ~ std_normal();
  xi_ethn_AND_age__AND_vote_sd ~ std_normal();

  to_vector(xi_ethn_AND_inco_AND_stat) ~ std_normal();
  xi_ethn_AND_inco_AND_stat_sd ~ std_normal();

  to_vector(xi_ethn_AND_inco_AND_vote) ~ std_normal();
  xi_ethn_AND_inco_AND_vote_sd ~ std_normal();

  to_vector(xi_ethn_AND_stat_AND_vote) ~ std_normal();
  xi_ethn_AND_stat_AND_vote_sd ~ std_normal();

  to_vector(xi_educ_AND_age__AND_inco) ~ std_normal();
  xi_educ_AND_age__AND_inco_sd ~ std_normal();

  to_vector(xi_educ_AND_age__AND_stat) ~ std_normal();
  xi_educ_AND_age__AND_stat_sd ~ std_normal();

  to_vector(xi_educ_AND_age__AND_vote) ~ std_normal();
  xi_educ_AND_age__AND_vote_sd ~ std_normal();

  to_vector(xi_educ_AND_inco_AND_stat) ~ std_normal();
  xi_educ_AND_inco_AND_stat_sd ~ std_normal();

  to_vector(xi_educ_AND_inco_AND_vote) ~ std_normal();
  xi_educ_AND_inco_AND_vote_sd ~ std_normal();

  to_vector(xi_educ_AND_stat_AND_vote) ~ std_normal();
  xi_educ_AND_stat_AND_vote_sd ~ std_normal();

  to_vector(xi_age__AND_inco_AND_stat) ~ std_normal();
  xi_age__AND_inco_AND_stat_sd ~ std_normal();

  to_vector(xi_age__AND_inco_AND_vote) ~ std_normal();
  xi_age__AND_inco_AND_vote_sd ~ std_normal();

  to_vector(xi_age__AND_stat_AND_vote) ~ std_normal();
  xi_age__AND_stat_AND_vote_sd ~ std_normal();

  to_vector(xi_inco_AND_stat_AND_vote) ~ std_normal();
  xi_inco_AND_stat_AND_vote_sd ~ std_normal();




  // VOTE CHOICE LIKELIHOOD
  for (i in 1:N) {
    //V[i] ~ categorical_logit(mu_s[i,1:J]');
    target += sum(V_star[i] .* log_softmax(mu_s[i,1:J]'));
  }

}
