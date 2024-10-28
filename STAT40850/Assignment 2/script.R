#load data
load(url("https://acaimo.github.io/teaching/data/foodexp.RData"))

x <- foodexp$income
y <- foodexp$food
n <- length(y)

library(ggplot2)
ggplot(foodexp, aes(x = income, y = food)) + geom_point(size = 0.5) 

#QUESTION 1

#we assume that daily income expenditure is 15% of household income
#see https://ec.europa.eu/eurostat/web/products-eurostat-news/w/ddn-20230201-1
n <- 1000
beta <- rbeta(n, 1, 7) #proportion of income spent on food around 15% - must be in (0, 1) i.e household spends between 0% and 100% of income on food
alpha <- rnorm(n, 0, 10) #random noninformative prior

means <- sapply(x, FUN=function(x) alpha+beta*x)

mu_hpdi_95 <- apply(means,
                    2,
                    function(x) quantile(x, c(0.025, 0.975)))
mu_hpdi_95_l <- mu_hpdi_95[1, ]
mu_hpdi_95_u <- mu_hpdi_95[2, ]

ggplot() +
  geom_point(data = foodexp, aes(x = income, y = food), shape = 19, cex = 0.5) +
  geom_abline(data = foodexp, aes(intercept = mean(alpha),
                                slope = mean(beta)), color = 'red')+
  geom_ribbon(aes(x, ymin = mu_hpdi_95_l, ymax = mu_hpdi_95_u),
              alpha = 0.3) +
  labs(subtitle = "95% posterior interval for mu")

#QUESTION 2
library(rstan)

stan_code <- '
data {
  int <lower=1> n;
  vector[n] y;
  vector[n] x;
}
parameters {
  real alpha;
  real beta;
  real sigma;
}
transformed parameters{ 
  vector[n] mu = alpha + beta * x;
}
model {
  beta ~ beta(1, 7);
  alpha ~ normal(0, 10);
  sigma ~ exponential(0.001);
  y ~ normal(mu, sigma);
}
generated quantities {
  vector[n] y_tilde;
  for (i in 1:n) y_tilde[i] = normal_rng(mu[i], sigma); 
}'

model <- stan_model(model_code = stan_code)

posterior <- sampling(model,
                      iter = 5000,
                      seed = 2,
                      data = list(n=n, x=x, y=y))
#print numeric summary
print(posterior, pars=c("alpha","beta","sigma"))
#plot distribution of parameters
library(bayesplot)
draws <- as.data.frame(posterior)
mcmc_pairs(draws, pars = c('alpha', 'beta', 'sigma'))
#create a 95% CI for y_tilde
y_tilde <- extract(posterior)$y_tilde
#create a 95% CI for each value of x (nb rows = prediction for new alpha, beta  col = prediction for x_i)
y_CI <- apply(y_tilde, MARGIN = 2, FUN = function (x) quantile(x, c(0.025, 0.975)))
y_CI_lower <- y_CI[1,]
y_CI_upper <- y_CI[2,]
ggplot() +
  geom_point(data = foodexp, aes(income, food), shape = 19, cex = 0.5)+
  geom_abline(data = foodexp, aes(intercept = mean(draws$alpha),
                                slope = mean(draws$beta)), color = 'red')+
  geom_ribbon(aes(x, ymin = y_CI_lower, ymax = y_CI_upper),
              alpha = 0.3)+
  labs(subtitle = "95% posterior interval for y_tilde")

#QUESTION 3
#PRIOR ESTIMATE
obs <- length(draws$beta)
beta <- rbeta(obs, 1, 7) 
alpha <- rnorm(obs, 0, 10)

mu_prior_50 <- alpha + beta*50
quantile(mu_prior_50, c(0.025, 0.5, 0.975))
#POSTERIOR ESTIMATE
mu_50 <- draws$alpha + draws$beta*50
quantile(mu_50, c(0.025, 0.5, 0.975))


#QUESTION 4
y_sampled <- sapply(x,
                   function(x) rnorm(dim(draws)[1],
                                    draws$alpha + draws$beta * x,
                                    draws$sigma))
pp_hpdi_95 <- apply(y_sampled,
                    2,
                    function(x) quantile(x, c(0.1, 0.9)))

pp_hpdi_95_l <- pp_hpdi_95[1, ]
pp_hpdi_95_u <- pp_hpdi_95[2, ]

ggplot() +
  geom_point(data = foodexp, aes(income, food), shape = 19) +
  geom_ribbon(mapping = aes(x,
                            ymin = pp_hpdi_95_l,
                            ymax = pp_hpdi_95_u), alpha = 0.3) +
  labs(subtitle = '95% posterior predictive interval')


#QUESTION 5
mu_63 <- draws$alpha + draws$beta * 63

ggplot() + 
  geom_density(aes(x = mu_63), fill = 'lightskyblue1') +
  labs(x = 'mu (expected rides a posteriori) | temp_feel x = 63')


plot(c(0),c(0), xlim=c(0,100), ylim=c(-100,100))
for (i in 1:1000)
{
  abline(a=alpha[i], b=beta[i], col = "lightgray")
}
par(new=T)
plot(foodexp$income,foodexp$food, xlim=c(0,100), ylim=c(-100,100))

