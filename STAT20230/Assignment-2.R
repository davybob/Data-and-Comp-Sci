#Load dataset
crime <-read.csv('Crimes.csv')

#Load library for fitting all models
library("olsrr")
#VR regressed on ALL covariates (.)
model <- lm(VR~ ., data=crime)
summary(fsModel)
#Fit all models
all_fitted_models <- ols_step_all_possible(model)
all_fitted_models
#Create table of models, adj r^2, aic, bic
table <- data.frame(predictor = all_fitted_models$predictors,
                    adjR2 = all_fitted_models$adjr,
                    AIC = all_fitted_models$aic,
                    BIC = all_fitted_models$sbc)
#Sort by increasing AIC
table <- table[order(table$AIC,decreasing = FALSE),]
table

#Find model with lowest AIC
minAIC <- which.min(table$AIC)
table[minAIC,]
#Find model with highest BIC
minBIC <- which.min(table$BIC)
table[minBIC,]
#Find model with highest R^2
minAdjR2 <- which.max(table$adjR2)
table[minAdjR2,]
crime

#Forward stepwise regression
forwardStep<- function(data){
  #Fit model with intercept
  fit <- lm(VR ~ 1, data=data);
  #Compute model BIC
  bic <- BIC(fit);
  #Get names of covariates to add into model
  covariates <- colnames(data)[-1];
  while (TRUE) {
    exit_c = TRUE
    best_model = fit
    best_covariate = ""
    #Loop through covariates and add to model, one-by-one
    print(covariates)
    for(covariate in covariates){
      #Update updates model with new formula! (+ -> add cov, - -> remove cov)
      new_fit <- update(fit, paste('.~.+',covariate));
      new_BIC <- BIC(new_fit);
      #Save model with lowest BIC
      if (new_BIC < bic){
        exit_c = FALSE;
        bic = new_BIC;
        best_model = new_fit;
        best_covariate = covariate
      }
    }
    #Remove covariate from list
    covariates = covariates[covariates != best_covariate]
    if (exit_c){
      break;
    }
    #Update model with model with lowest BIC
    fit = best_model;
  }
  fit 
}

Run forward-stepwise regression on crime data
fit <- forwardStep(crime)
colnames(fit$model)
table

#Forward stepwise found the best possible subset that minimizes BIC
#Forward stepwise may not find the optimal solution because it doesn't evaulate
#All possible models, only a subset of all possible models. For this reason,
#Forward stepwise can miss certain subsets.

#Backward stepwise regression
backwardStep<- function(data){
  #Fit model with all covariates
  fit <- lm(VR ~ ., data=data);
  covariates <- colnames(data)[-1];
  p = length(length(fit$coefficients)-1)
  n = nrow(data)
    while (TRUE) {
    exit_c = TRUE
    best_model = fit
    #Calculate model's MSE
    SSE = sum(fit$residuals^2)
    MSE = SSE/(n-p-1)
    p_val = 1
    #Loop through covariates and remove from model, one-by-one
    for(covariate in covariates){
      #Update updates model with new formula! - -> remove covariate
      new_fit <- update(fit, paste('.~.-',covariate));
      SSE_New <- sum(new_fit$residuals^2)
      SSE_Extra <- SEE_New - SSE
      F_new <- (SSE_New-SSE)/MSE;
      p.star = 1-pf(F_new,1,n-p-1);
      #Save model with lowest BIC
      if (p.star < p_val){
        exit_c = FALSE;
        p_val = p.star
        best_model = new_fit;
        best_covariate = covariate
      }
    }
    #Remove covariate from list
    covariates = covariates[covariates != best_covariate]
    if (exit_c){
      break;
    }
    #Update model with model with lowest BIC
    fit = best_model;
    p = p - 1
    print(p.star)
  }
  fit 
}
#Run backward stepwise regression
backwardStep(crime)

model <- lm(VR~ ., data=crime)
summary(update(model,'.~.-MR'))

z <- summary(lm(VR ~ MR+M, data=crime))
z

#------------------------------------------------------------------------------#
#Load football dataset
football <- read.csv('football.csv')
#fit model
model <- lm(y~x2+x7+x8,data=football)
summary(model)
y = football$y
y_hat <- model$fitted.values
y_bar <- mean(y)
SST <- sum((y-y_bar)^2)
SSE <- sum((y-y_hat)^2)
SSR <- sum((y_hat-y_bar)^2)

#SSR=SSR+SSE
#df(SSR) = df(SSR)+df(SSE)=n − p − 1 + p = n-1

n=length(y)
p=3
MST=SST/(n-1)
MSE=SSE/(n-p-1)
MSR=SSR/p

x <- model.matrix(model)
hat <- solve(t(x)%*%x)
sigma <- MSE
covar <- sigma*hat
df <- n-p-1
#beta_1
beta <- as.numeric(model$coefficients[2])
var.beta <- sqrt(covar[2,2])
t.star <- beta/var.beta
t.start <- abs(t.star)
1-pt(t.star, df)
#beta_2
beta <- as.numeric(model$coefficients[3])
var.beta <- sqrt(covar[3,3])
t.star <- beta/var.beta
t.start <- abs(t.star)
1-pt(t.star, df)
#beta_3
beta <- as.numeric(model$coefficients[4])
var.beta <- sqrt(covar[4,4])
t.star <- beta/var.beta
t.star <- abs(t.star)
1-pt(t.star, df)

#d
r.squared = 1-MSE/MST
adj.r.squared <- 1-(1-r.squared)*(n-1)/df

#e
F.star = MSR/MSE
F.crit <- qf(0.05,df,p)
F.star > F.crit

#f
cor(y,y_hat)^2
r.squared

#g
predict(model,data.frame(x2=2300,x7=56,x8=2100), interval='confidence')
#h
fit_new <- update(model, '.~.-x2')
y_hat <- fit_new$fitted.values
SSE <- sum((y-y_hat)^2)
SSR <- sum((y_hat-y_bar)^2)

#i
MSE=SSE/(n-2-1)
MSR=SSR/2
MST=SST/(n-1)

F.star = MSR/MSE
F.crit <- qf(0.05,df,p)
F.star > F.crit

#j
r.squared <- 1-MSE/MST
adj.r.squared <- 1-(1-r.squared)*(n-1)/df
r.squared
adj.r.squared
mn
#k
predict(fit_new,data.frame(x7=56,x8=2100), interval='confidence')

#Removing x2 increased the error in the model - larger interval + worse fit
#Less of the variation in the response is captured in the model
#------------------------------------------------------------------------------#
#Load bikesharing dataset
bike <- read.csv('bikesharing.csv')

#Use `factor` to conver strings to categories
bike$mnth <- factor(bike$mnth,
                    unique(bike$mnth),
                    labels = c('january','february','march','april','may',
                               'june','july','august','september','october',
                               'november','december'))
bike$mnth <- relevel( bike$mnth, ref = "january")

#b
fit <- lm(cnt ~ hum + windspeed + temp + mnth,data=bike)
summary(fit)
levels(bike$mnth)
#based on summary we see some months are related and some months are not (autumn/winter months very related, summer/spring not so)

samplePred <- data.frame(hum=rep(0.4,12),windspeed=rep(0.3,12),temp=rep(0.65,12),
                         mnth=factor(levels(bike$mnth)))
table <- data.frame(predictions = predict(fit,samplePred), row.names = levels(bike$mnth))
t(table)
