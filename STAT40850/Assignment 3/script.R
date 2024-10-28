# Priors are normally distributed from of regression theory
# Compare skeptically priors graphically, add to appendix

load(url("https://acaimo.github.io/teaching/data/italian_wines.RData"))
head(italian_wines)
r<-lm(alcohol~.,data=italian_wines)$residuals
plot(scale(italian_wines$alcohol))
# Come up with some reasonable prior 
# a ~ N(12, 5) <- average alcohol content in wine is 11.5%
# We don't know what effect, if any, the predictors have on alcohol content
# we suspect they should have small effects if any
# So we set them to vague priors
# They should not have large affects on the alcohol content
# range of predictors is close to range of response => no larger than |2|.
library(rstan)
stancode <- 
  '
data {
  //number of samples
  int <lower=1> n;
  //responses
  vector[n] y;
  //predictors - all are added into model
  //We set different models by inputting vector of zeros for col or pro
  vector[n] mag;
  vector[n] col;
  vector[n] pro;
}
parameters {
  real alpha;
  real beta_1;
  real beta_2;
  real beta_3;
  real sigma;
}
transformed parameters{ 
  vector[n] mu = alpha + beta_1 * mag + beta_2 * col + beta_3 * pro;
}
model {
  //flat prior for intercept, centered around avg alcohol content
  alpha ~ normal(11.5, 5);
  //true parameter values most likely small, given the range of response/preds
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  sigma ~ exponential(2); 
  y ~ normal(mu, sigma);
}
generated quantities {
  vector[n] y_tilde;
  vector[n] log_lik;
  for (i in 1:n) {
    y_tilde[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
  }
}
'
#Create stan model
stan.model <- stan_model(model_code = stancode)
#Sample from model 1 - set color and proline to vector of zeros
#Parameters b_2,b_3 will still be estimated but will have no effect on mu
#or y_tilde
#The computation is slower but less code is used
model_mag <- sampling(stan.model, iter = 5000, seed = 1, 
                   data = list(n = nrow(italian_wines), 
                               y = italian_wines$alcohol, 
                               mag = italian_wines$magnesium,
                               col = 0*italian_wines$color_intensity,
                               pro = 0*italian_wines$proline))
model_magcol <- sampling(stan.model, iter = 5000, seed = 1, 
                         data = list(n = nrow(italian_wines), 
                                     y = italian_wines$alcohol, 
                                     mag = italian_wines$magnesium,
                                     col = italian_wines$color_intensity,
                                     pro = 0*italian_wines$proline))
model_magcolpro <- sampling(stan.model, iter = 5000, seed = 1, 
                            data = list(n = nrow(italian_wines), 
                                        y = italian_wines$alcohol, 
                                        mag = italian_wines$magnesium,
                                        col = italian_wines$color_intensity,
                                        pro = italian_wines$proline))
#Comment on how beta_1 changes in each model
#model 1 - positive but large range (0.6,0.33)
#model 2 - positive but large range (0.03,0.24) - b2 positive, small range
#model 3 - overlaps with origin suggesting no relationship - b2,b3 positive, small range
#suggested b2 and b3 are most likely to be related to alcohol but b1 not so.
#What do predictors mean? Unit increase in Blah = unit increase/decrease in blah
#Extract matrix and plot density plots for each predictor
print(model_mag, pars = c('alpha', 'beta_1',  'sigma'))
print(model_magcol, pars = c('alpha', 'beta_1', 'beta_2', 'sigma'))
print(model_magcolpro, pars = c('alpha', 'beta_1','beta_2', 'beta_3', 'sigma'))

#Evaluate model output in each case (plot y, plot residuals), (use density plot)
library(ggplot2)
y_mag <- as.matrix(model_mag, pars = c("y_tilde"))
y_mag_qt <- apply(y_mag, 2, function(x) quantile(x, c(0.025, 0.975)))
pp_hpdi_95_l <- y_mag_qt[1, ]
pp_hpdi_95_u <- y_mag_qt[2, ]
d.plot <- data.frame(x = 1:nrow(italian_wines),y=italian_wines$alcohol)
ggplot()+
geom_point(data = d.plot,
           aes(x= x,y = y), shape = 19, cex = 0.5)+
ggtitle("95% posterior predictive interval")+
geom_ribbon(data = d.plot,
            mapping = aes(x,
                          ymin = pp_hpdi_95_l,
                          ymax = pp_hpdi_95_u), alpha = 0.4)
#re-use d.plot dataframe for plotting mean residual
for (i in 1:178){
  d.plot$y[i] <- mean(y_mag[,i]-italian_wines$alcohol[i])   
}
ggplot(d.plot, aes(x=x, y=y))+
  geom_point(shape = 19, cex = 1)+
  ggtitle("Average Residuals")+
  geom_smooth(se=FALSE, colour="red", span=0.3, linetype="11")


y_magcol <- as.matrix(model_magcol, pars = c("y_tilde"))
y_magcolpro <- as.matrix(model_magcolpro, pars = c("y_tilde"))
ppc_dens_overlay(italian_wines$alcohol, y_magcol[1:1000, ])
ppc_dens_overlay(italian_wines$alcohol, y_magcolpro[1:1000, ])
#Outlier 

#Compare model fit with density plot + pair plot of 95% CI? or R^2 statistic?

#Likely magnesium not related to alcohol content, ditch and compare with best model found.


library(loo)
# Compare with WAIC and LOO 
#elpd_loo
loo_mag <- loo(model_mag)
loo_magcol <- loo(model_magcol)
loo_magcolpro <- loo(model_magcolpro)
loo_compare(list('Model Mag' = loo_mag, 
                 'Model Mag+Col' = loo_magcol,
                 'Model Mag+Col+Pro' = loo_magcolpro))
#WAIC
log_lik_mag <- extract_log_lik(model_mag)
waic_mag <- waic(log_lik_mag)
log_lik_magcol <- extract_log_lik(model_magcol)
waic_magcol <- waic(log_lik_magcol)
log_lik_magcolpro <- extract_log_lik(model_magcolpro)
waic_magcolpro <- waic(log_lik_magcolpro)

loo_compare(list('Model Mag' = waic_mag, 
                 'Model Mag+Col' = waic_magcol,
                 'Model Mag+Col+Pro' = waic_magcolpro))

#Alternative model - col+pro, fit + compare with best out of bunch
model_new <- sampling(stan.model, iter = 5000, seed = 1, 
                      data = list(n = nrow(italian_wines), 
                                  y = italian_wines$alcohol, 
                                  mag = 0*italian_wines$magnesium,
                                  col = italian_wines$color_intensity,
                                  pro = italian_wines$proline))
print(model_new, pars = c('alpha', 'beta_2', 'beta_3',  'sigma'))
y_tilde <- as.matrix(model_new, pars = c("y_tilde"))
y_tilde_qt <- apply(y_tilde, 2, function(x) quantile(x, c(0.025, 0.975)))
pp_hpdi_95_l <- y_tilde_qt[1, ]
pp_hpdi_95_u <- y_tilde_qt[2, ]
d.plot <- data.frame(x = 1:nrow(italian_wines),y=italian_wines$alcohol)
ggplot()+
  geom_point(data = d.plot,
             aes(x= x,y = y), shape = 19, cex = 0.5)+
  ggtitle("95% posterior predictive interval")+
  geom_ribbon(data = d.plot,
              mapping = aes(x,
                            ymin = pp_hpdi_95_l,
                            ymax = pp_hpdi_95_u), alpha = 0.4)
#re-use d.plot dataframe for plotting mean residual
for (i in 1:178){
  d.plot$y[i] <- mean(y_tilde[,i]-italian_wines$alcohol[i])   
}
ggplot(d.plot, aes(x=x, y=y))+
  geom_point(shape = 19, cex = 1)+
  ggtitle("Average Residuals")+
  geom_smooth(se=FALSE, colour="red", span=0.3, linetype="11")
