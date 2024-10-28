### QUESTION 1 ###
# Load Data
Temp_data <- read.csv("Temperature_data.csv")
# Set random seed for reproducability
set.seed(18306686)
# Select 1000 random subsets from dataset
data <- Temp_data[sample(1:nrow(Temp_data), 1000),]
### QUESTION 2 ###
# Remove NA observations
data <- data[complete.cases(data),]
# Convert char to factor
data$Ethnicity <- as.factor(data$Ethnicity)
data$Age <- ordered(data$Age, levels=c("18-20", "21-25", "21-30", "26-30",
                                       "31-40", "41-50", "51-60", ">60"))
data$Gender <- as.factor(data$Gender)
data$pyrexic <- as.factor(data$pyrexic)
#Remove observations below 4 sd from mean
m <- mean(data[,11])
s <- sd(data[,11])
data <- data[(m - data[,11]) > -4*s,]
#Split into Predictors and Responses
data.pred <- data[,1:11]
data.resp <- data[,12:ncol(data)]
# Visualize
col <- c("blue","red")
cols <- col[as.numeric(data.resp$Gender[!duplicated(data.pred)])]
pairs(data.pred, col=cols)
library(Rtsne)
plot(Rtsne(data.pred[!duplicated(data.pred),],perplexity=20)$Y,
     col = col, pch=19)
#Comment - very high correlation between variables!
corr <- as.matrix(cor(data.pred))
corrplot::corrplot(corr,
                   method = 'number', 
                   number.digits = 2)
### QUESTION 3 ###
#We standardize the data so that clustering is better
#This is equivalent to replacing the variables with dimensionless quantities
data.pred <- as.data.frame(scale(data.pred)) #1-10 are facial temperature values (11 is oral)
#This is the best combination of clustering parameters
#There are 2-3 distinct clusters illustrated in the dendrogram
dend.best <- hclust(dist(data.pred, method="euclidean"), method="complete")
plot(dend.best, xlab="Clusters", 
     ylab="Height",
     main="Cluster Dendrogram 1")
dend_best <- cutree(dend.best, k = 2)
col <- c("blue", "red")
cols <- col[dend_best]
plot(data.pred$aveOralM, 
     col=cols, pch=19, xlab = "Index", ylab="Oral Temperature (std from mean)",
     main="Standardized Oral Temperature Coloured By Hierachical Clustering")
#Using other clustering parameters shows different clustering
#Particularly some outlier points
dend.t1 <- hclust(dist(data.pred, method="euclidean"), method="average")
plot(dend.t1)
dend_t1 <- cutree(dend.t1, k = 4)
plot(data.pred$aveOralM, col=dend_t1)

dend.t2 <- hclust(dist(data.pred, method="euclidean"), method="single")
plot(dend.t2)
dend_t2 <- cutree(dend_t2, k = 30)
plot(data.pred$aveOralM, col=data.resp$groups)

dend.t3 <- hclust(dist(data.pred, method="maximum"), method="complete")
dend_t3 <- cutree(dend.t3, k = 3)

plot(dend.t3)

dend.t3 <- hclust(dist(data.pred, method="euclidean"), method="average")
plot(dend.t3)


dend_t3 <- cutree(dend.t3, k = 3)
plot(data.pred$aveOralM, col=dend_t3)

#Best KM centers between 1 and 10. We'll fit Kmeans to these number of cluster
#And see where the 'elbow' occurs
K <- 10
withiness <- rep(0,K)
#Compute one-cluster solution
withiness[1] = (nrow(data.pred)-1) * sum(apply(data.pred, 2, var))
for (i in 2:K) {
  km <- kmeans(data.pred, centers=i)
  withiness[i] <- sum(km$withinss)
}
#Plot shows "elbow" occuring at 2-3 clusters
plot(withiness, type='b', col="blue", 
     main="Sums of Squares Against Number of Clusters")
best_km <- kmeans(data.pred, centers=2)
# Now we compare clustering solution using classAgreement
library(e1071)
plot(data.pred$aveOralM, 
     col=best_km$cluster,
     pch=19,
     xlab="Oral Temperature (std from mean)", 
     ylab="Observation",
     main="Oral Temperature Coloured by K-means Clustering"
)
plot(data.pred$aveOralM, 
     col=dend_best,
     pch=19,
     xlab="Oral Temperature (std from mean)", 
     ylab="Observation",
     main="Oral Temperature Coloured by Hierarchical Clustering"
)
tbl <- table(cutree(dend.best, k = 2), best_km$cluster)
sum(diag(tbl))/sum(tbl)
classAgreement(tbl)$rand
### QUESTION 3 ###
Accuracy <- function(c1, c2) {
  num <- sum(diag(table(c1, c2)))
  dim <- sum((table(c1, c2)))
  num/dim
}

library(MASS)
#Perform fit with CV
lda.resp = lda(x=data.pred, grouping=data.resp$Gender, CV=TRUE)
# Perform LOOCV on QDA model
qda.resp = qda(x=data.pred, grouping=data.resp$Gender, CV=TRUE)
# Create confusion matrix for LDA model
tbl.lda <- table(lda.resp$class, data.resp$Gender)
# Compute miss-classification rate of LDA model
misclass.lda <- 1-sum(diag(tbl.lda))/sum((tbl.lda))
# Create confusion matrix for QDA model
tbl.qda <- table(qda.resp$class, data.resp$Gender)
# Compute miss-classification rate of LDA model
misclass.qda <- 1-sum(diag(tbl))/sum((tbl))
# Compute pca for dataset and subset first two PCs
pca.two <- prcomp(data.pred)$x[,1:2]
# Compute lda with PCs 
lda.resp.pca <- lda(x=as.matrix(pca.two), grouping=data.resp$Gender)
# Function to plot boundary
boundary <- function(model, data, class = NULL, predict_type = "class",
                     resolution = 100, showgrid = TRUE, col=NULL, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  k <- length(unique(cl))
  
  plot(data, col = col, pch = 19, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
col <- c("red", "blue")
cols <- col[data.resp$Gender]
boundary(lda.resp.pca, as.data.frame(pca.two), 
         col = cols,main = "LDA")


lda.resp.pca <- lda(x=as.matrix(pca.pred$x[,1:2]), grouping=data.resp$Gender)

boundary <- function(model, data, class = NULL, predict_type = "class",
                     resolution = 100, showgrid = TRUE, col=NULL, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  k <- length(unique(cl))
  
  plot(data, col = col, pch = 19, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
col <- c("red", "blue")
cols <- col[data.resp$Gender]
boundary(lda.resp.pca, as.data.frame(pca.pred$x[,1:2]), 
         col = cols,main = "LDA")

### QUESTION 5 ###
library(ggplot2)
# Bootstrap procedure (resample .75 of data)
data.new <- data.pred[sample(1:nrow(data.pred), size = round(nrow(data.pred)*0.75),
                            replace=TRUE),]
pca.pred.new <- prcomp(data.new)
pca.pred <- prcomp(data.pred)
round((pca.pred$sdev^2) / (pca.pred.new$sdev^2),3)

ratios <- matrix(rep(NA, times=nrow(data.pred)*ncol(data.pred)),  nrow = nrow(data.pred))
#Jack-knife approach
for (i in 1:nrow(data.pred)){
  p <- prcomp(data.pred[-i,])
  ratios[i,] <- p$sdev^2/pca.pred$sdev^2
}
colMeans(ratios[i,])

# Compute proportion of variance explained by each PC
pca.pred$prop <- pca.pred$sdev^2/sum(pca.pred$sdev^2)
# Compute cumulative proportion of variance explained by each PC
pca.pred$cumprop <- cumsum(pca.pred$prop)
# Plot cumulative proportion 
d.model <- data.frame(x=seq(1,length(pca.pred$prop)), y=pca.pred$cumprop)
ggplot(d.model, aes(x=x, y=y)) +
  geom_point()+ 
  geom_line()+
  scale_x_continuous(breaks = seq(1,length(d.model$x))) +
  scale_y_continuous(breaks = round(seq(min(d.model$y), max(d.model$y)+1, by = 0.05),2))+
  xlab("# Principal Components")+
  ylab("Cumulative Variance")+
  ggtitle("Cumulative Variance Explained by Number of Principal Components")

#Choose two PC as best
c <- var(data.pred)
# Eigenvector decomposition, subset to get two eigenvectors
decomp <- eigen(c)$vectors[,1:2]
# Compute scores
scores <- -1*as.matrix(data.pred)%*%as.matrix(decomp)
col <- c("blue", "red")
cols <- col[as.numeric(data.resp$pyrexic)]
plot(scores, 
     col=cols, 
     pch=19,
     xlab = "PC 1",
     ylab = "PC 2",
     main = "Principal Component Scores for Each Observation")
legend()


# PCR library
library(pls)
# Choose .75 train .25 test
N <- nrow(data.pred)
#Note data has been standardized to prevent dimensions with large variance dominating - i.e. all variables on same scale
tr <- sample(1:N, round(N*0.75))
train <- data.pred[tr,]
test <- data.pred[-tr,]
#Fit PCR model with all 10 components (PCs) - on training dataset
yoral.pcr <- pcr(aveOralM ~ ., 10, data = train)
# Plot RMSEP to visualize decrease on increasing PCs
# illustrated is going beyond 2 components is not needed
plot(RMSEP(yoral.pcr))
#CV procedure to tune components
yoral.cv <- crossval(yoral.pcr, segments = 10)
plot(MSEP(yoral.cv), legendpos = "topright")
summary(yoral.cv, what = "validation")

yoral.pcr <- pcr(aveOralM ~ ., 2, data = train)
RMSEP(yoral.pcr, newdata = test)
summary(yoral.pcr)
y <- test$aveOralM
y_hat <- predict(yoral.pcr, ncomp = 2, newdata = test)
plot(y,y_hat,
     xlab="Measured",
     ylab="Predicted",
     main="IRT dataset, 2 PCs, test data")
lobf <- lm(y~y_hat)$coeff
abline(a=lobf[1], b = lobf[2], col = "red")
legend("topleft", col="red", legend="Best Fit Line", lty=1)
