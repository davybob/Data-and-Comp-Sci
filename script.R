#load dataset
load(url("https://acaimo.github.io/teaching/data/referendum.RData"))

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#----- Pooled Model -----#
#inverse logit / sigmoid
sigmoid <- function(theta) exp(theta)/(1+exp(theta))
#logit / inverse sigmoid
logit <- function(p) log(p/(1-p))
#pooled model data
n_i <- referendum$n_votes
x_i <- referendum$n_yes
n <- length(referendum$n_votes)

stan_code_pooled <- 
'
data{
  int<lower=0> n;
  int<lower = 0> x_i[n];
  int<lower = 0> n_i[n];
}
parameters{
  real mu;
  real<lower=0> sigma;
  real theta;
} 
model{
  mu ~ normal(0, 5);
  sigma ~ exponential(0.01);
  theta ~ normal(mu, sigma);
  for (i in 1:n) {
    x_i[i] ~ binomial_logit(n_i[i], theta);
  }
}
generated quantities{
  vector[n] log_lik;
  vector[n] theta_i_tilde;
  for (i in 1:n) {
    log_lik[i] = binomial_logit_lpmf(x_i[i] | n_i[i], theta);
    theta_i_tilde[i] = normal_rng(mu, sigma);
  }
}
'
pooled_model <- stan_model(model_code = stan_code_pooled)
pooled_posterior <- sampling(pooled_model, iter = 5000, seed = 1, 
                             data = list(n = n, 
                                         x_i = x_i, 
                                         n_i = n_i))
post_summary <- summary(pooled_posterior, probs = c(0.025, 0.975), 
                        pars = c('mu', 'sigma'))$summary

round(post_summary, 2)
#plot mean line along with lollipop plot of individual consitituency votes

#--------Hierarchical Model -----#
x_ij <- referendum$n_yes
n_i <- referendum$n_votes
j <- referendum$constituency
m <- length(table(referendum$constituency))

stan_code_hierarchical <- '
data{
  int<lower=0> n;         
  int<lower=0> m; //number of constituencies
  int<lower=0, upper=m> j[n];  // assignment j for each observation i
  int<lower=0> x_ij[n];    
  int<lower=0> n_i[n];
}
parameters{
  real mu;
  real<lower=0> sigma;
  real theta_j[m];
}
model{
  mu ~ normal(0, 5);
  sigma ~ exponential(0.01);
  theta_j ~ normal(mu, sigma);
  for (i in 1:n) {
    x_ij[i] ~ binomial_logit(n_i[i], theta_j[ j[i] ] );
  }
}
generated quantities{
  vector[n] log_lik;
  vector[n] theta_i_tilde;
  for (i in 1:n) {
    log_lik[i] = binomial_logit_lpmf(x_ij[i] | n_i[i], theta_j[ j[i] ]);
    theta_i_tilde[i] = theta_j[ j[i] ];
  }
}
'
hierarchical_model <- stan_model(model_code = stan_code_hierarchical)
hierarchical_posterior <- sampling(hierarchical_model, iter = 8000, seed = 1, 
                             data = list(n = n, 
                                         m=m,
                                         j=j,
                                         x_ij = x_ij, 
                                         n_i = n_i))
hierarchical_summary <- summary(hierarchical_posterior, probs = c(0.025, 0.975), 
                        pars = c('mu', 'sigma'))$summary

round(hierarchical_summary, 2)

#--------Hierarchical Model Version 2 (Const + Region) -----#
x_ij <- referendum$n_yes
n_i <- referendum$n_votes
c <- referendum$constituency
r <- referendum$region
size_c <- length(table(referendum$constituency))
size_r <- length(table(referendum$region))

stan_code_hierarchical_cr <- '
data{
  int<lower=0> n;         
  int<lower=0> size_c; //number of constituencies
  int<lower=0> size_r; //number of regions
  int<lower=0, upper=size_c> c[n];  // constituency assignment for each observation i
  int<lower=0, upper=size_r> r[n];  // region assignment for each observation i
  int<lower=0> x_ij[n];
  int<lower=0> n_i[n];
}
parameters{
  //constituent differences
  real mu_r[size_r];
  real<lower=0> sigma;
  //regional differences
  real mu;
  real<lower=0> tau;
  //logit transforms for each constituency
  real theta_c[size_c];
}
model{
  //regional probabilities
  mu ~ normal(0, 5);
  tau ~ exponential(2);
  //constituent probabilities
  mu_r ~ normal(mu, tau);
  sigma ~ exponential(2);
  
  for (i in 1:n) {
    theta_c[c[i]] ~ normal(mu_r[ r[i] ], sigma);
    x_ij[i] ~ binomial_logit(n_i[i], theta_c[ c[i] ]);
  }
}
generated quantities{
  vector[n] log_lik;
  vector[n] theta_i_tilde;
  for (i in 1:n) {
    log_lik[i] = binomial_logit_lpmf(x_ij[i] | n_i[i], theta_c[ c[i] ]);
    theta_i_tilde[i] = theta_c[c[i]];
  }
}
'
hierarchical_model_cr <- stan_model(model_code = stan_code_hierarchical_cr)
hierarchical_posterior_cr <- sampling(hierarchical_model_cr, iter = 8000, seed = 1, 
                                   data = list(n = n, 
                                               size_c = size_c,
                                               size_r = size_r,
                                               c=c,
                                               r=r,
                                               x_ij = x_ij, 
                                               n_i = n_i))
hierarchical_cr_summary <- summary(hierarchical_posterior_cr, probs = c(0.025, 0.975), 
                                pars = c('mu', 'tau'))$summary
round(hierarchical_summary, 2)

#---GOODNESS OF FIT TEST---#
library(bayesplot)
theta_i <- logit(x_i/n_i)
theta_i_tilde <- extract(pooled_posterior)$theta_i_tilde
ppc_ribbon(theta_i, theta_i_tilde[1:1000, ], prob_outer = 0.95, 
           alpha = 0.9, y_draw = 'points') +
  ggplot2::xlab('i') +
  ggplot2::ylab(expression(paste(x[i]))) + 
  legend_none()
ppc_dens_overlay(theta_i, theta_i_tilde[1:1000, ]) +
  ggplot2::xlab(expression(paste(x[i]))) +
  ggplot2::ylab('Density') + 
  legend_none()
theta_i_tilde <- extract(hierarchical_posterior)$theta_i_tilde

ppc_ribbon(theta_i, theta_i_tilde[1:1000, ], prob_outer = 0.95, 
           alpha = 0.9, y_draw = 'points') +
  ggplot2::xlab('Observation i') +
  ggplot2::ylab(expression(paste(x[i]))) + 
  legend_none()

ppc_dens_overlay(x_ij, x_ij_tilde[1:1000, ]) +
  ggplot2::xlab(expression(paste(x[i]))) +
  ggplot2::ylab('Density') + 
  legend_none()

theta_i_tilde <- extract(hierarchical_posterior_cr)$theta_i_tilde
ppc_ribbon(theta_i, theta_i_tilde[1:1000, ], prob_outer = 0.95, 
           alpha = 0.9, y_draw = 'points') +
  ggplot2::xlab('i') +
  ggplot2::ylab(expression(paste(x[i]))) + 
  legend_none()


#--LOO-CV--#
library(loo)

P_loo <- loo(pooled_posterior)
H1_loo <- loo(hierarchical_posterior)
H2_loo <- loo(hierarchical_posterior_cr)

loo_compare(list('Pooled model' = P_loo,
                 'Hierarchical T1 models' = H1_loo,
                 'Hierarchical T2 model' = H2_loo))
