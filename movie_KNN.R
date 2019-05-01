getwd()
setwd("/Volumes/Hyundew/Dewwww/빅데이터 장학금")

# making target variable 
movie <- read.csv("movie.csv", header=T)
soon <- movie$gross/movie$duration
soon2 <- log(soon) 
yvar <- ifelse(soon2 >= summary(soon2)[5],1,0)
y <- as.factor(yvar)
movie <- movie[,-c(2,4)]
movie <- cbind(movie, y)
movie$budget <- movie$budget/1000000

# scale
movie[,-9] <- scale(movie[,-9])


set.seed(7)
V=2
n= nrow(movie)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
movie.train = movie[ii,]
movie.test  = movie[-ii,]



### knn
#install.packages("class")
library(class)

# performing knn k from 1 to 10
for (i in 1:10){
  fit.knn = knn(train=movie.train[,-9],test=movie.test[,-9],cl=movie.train[,9], k=i, prob=T) 
  yhat = fit.knn
  ctable = table(movie.test[,9], yhat, dnn=c("Actual", "Predicted"))
  miss.err = 1-sum(diag(ctable))/sum(ctable)
  pred.acc = 1 - miss.err; round(pred.acc, 3)
  print(pred.acc)
}

# KNN when k=9
fit.knn.9 = knn(train=movie.train[,-9], test=movie.test[,-9], cl=movie.train[,9], k=9, prob=T)

# prediction
yhat.9 = fit.knn.9
ctable = table(movie.test[,9], yhat.9, dnn=c("Actual", "Predicted")); ctable #classification table

# errors
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy

library(ROCR)
prob <- attr(fit.knn.9, "prob")
pred = prediction(prob, movie.test$y)
perf = performance(pred, "tpr","fpr")
par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("KNN when k=9","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
performance(pred, "auc")@y.values #AUC


# 5-fold CV - model assessment by resampling
V = 5   
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5
set.seed(12345)
id = sample(1:V, nrow(movie.test), replace = T)

for(i in 1:V) {
  movie.train = movie[id != i,] 
  movie.test = movie[id == i,] 
  fit = knn(train=movie.train[,-9], test=movie.test[,-9], cl=movie.train[,9], k=9) 
  yhat=fit
  miss.err.test = miss.err.test + mean(movie.test[,9] != yhat) 
}
cv.err.test = miss.err.test/ V;cv.err.test 

# 10-fold CV - model assessment by resampling
V = 10   
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5
set.seed(12345)
id = sample(1:V, nrow(movie.test), replace = T)

for(i in 1:V) {
  movie.train = movie[id != i,] 
  movie.test = movie[id == i,] 
  fit = knn(train=movie.train[,-9], test=movie.test[,-9], cl=movie.train[,9], k=9) 
  yhat=fit
  miss.err.test = miss.err.test + mean(movie.test[,9] != yhat) 
}
cv.err.test = miss.err.test/ V;cv.err.test


