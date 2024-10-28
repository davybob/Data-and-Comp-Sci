#pre-requsite
nA <- 296
xA <- 169

nB <- 380
xB <- 247

#Q1 
#model parameters 
alpha <- 13
beta <- alpha*7/13
#prior distribution |  theta ~ Beta(alpha, beta)
#weak posterior do to lack of prior information
library(ggplot2)
x <- data.frame(seq(0,1,0.01))
dbeta(1, alpha, beta)
f <- data.frame(prior = dbeta(x, alpha, beta),
                posterior = dbeta(x, xA+alpha, nA-xA+beta))
ggplot()+
  stat_function(color="black",
                geom = "polygon",
                fill="green",
                alpha=0.25,
                fun = function(x) dbeta(x, alpha,beta))+
  stat_function(fun = function(x) dbeta(x, xA+alpha, nA-xA+beta),
                color="black",
                geom = "polygon",
                fill="yellow",
                alpha=0.25)+
  labs(title="Prior & Posterior Density Plots",
       x=expression(theta),
       y="Density")+
  scale_y_continuous(breaks = round(seq(min(dat$y), 
                    max(dat$y), by = 0.5),1))

#Q2
#posterior distribution | theta|x ~ Beta(x+alpha, n-x+beta)
#we'll plot prior, likelihood, then posterior
curve(dbeta(x, xA+alpha, nA-xA+beta),add=T)
#Q3
#Use hypothesis test (plot line on plot)
#comment on whether theta is in the interval or not!
#hypothesis test 
#theta_A = 0.6
#theta_A != 0.6
c(qbeta(0.025, xA+alpha, nA-xA+beta),
  qbeta(0.975, xA+alpha, nA-xA+beta))

#Fail to reject H0
# => there is evidence that theta=0.6

#sample from posterior distribution 
samples <- 100
theta_samples <- rbeta(samples,xA+alpha1, nA-xA+beta1)
#use posterior distribution samples (theta_s) to sample from 
# posterior predictive distribution x_bar ~ binom(n, theta_s)
x_bar <- rbinom(samples, nA, theta_samples)
hist(x_bar)
summary(x_bar)
quantile(x_bar, probs = c(0.025, 0.5, 0.975))


##PART 2 - using STAN
stancode <- 
'
data {
    int <lower=0> nA;
    int <lower=0, upper=nA> xA;

    int <lower=0> nB;
    int <lower=0, upper=nB> xB;

    int <lower=0> n_sim;
  }
  parameters {
    real <lower=0, upper=1> thetaA;
    real <lower=0, upper=1> thetaB;
  }
  model{
    thetaA ~ beta(3,2);
    thetaB ~ beta(3,2);
    xA ~ binomial(nA,thetaA);
    xB ~ binomial(nB,thetaB);
  }
  generated quantities {
    real theta_diff = thetaB-thetaA;
  }  
'
BB_model <- stan_model(model_code = stancode)

BB_posterior <- sampling(BB_model,
                         iter = 1500,
                         seed = 1,
                         data = list(nA=nA, xA=xA, 
                                     nB=nB, xB=xB,
                                     n_sim=100))
print(BB_posterior, pars = c('thetaA', 'thetaB', 'theta_diff'))

library(bayesplot)

posteriors <- as.data.frame(BB_posterior)
mcmc_hist(posteriors, pars = c('thetaA', 'thetaB', 'theta_diff'))
