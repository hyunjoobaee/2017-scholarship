


###############################################
# data준비       #
movie<-read.csv("C:\\Users\\KIMEUNGCHAE\\Desktop\\3-2\\stat\\movie.csv",header=TRUE)


##target 변수 만들기 : soon = gross/duration
soon <- movie$gross/movie$duration
soon2<-log(soon) 
yvar<-ifelse(soon2 >= summary(soon2)[5],1,0)
y<-as.factor(yvar)
movie<-movie[,-c(2,4)]
movie <- cbind(movie, y)
movie$budget<-movie$budget/1000000




###############################################
# Lab: LDA/QDA with German Credit Data        #
###############################################


### Install packages

install.packages("MASS")
library(MASS)


### LDA

fitlda = lda(y ~., data=movie)
plot(fitlda)
fitqda = qda(y ~., data=movie)
plot(fitqda)


### Prediction

cutoff = 0.5
pred = predict(fitlda, newdata=movie)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(movie$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

install.packages("ROCR")
library(ROCR)
pred2 = predict(fitlda, newdata=movie)$posterior
pred = prediction(pred2[,2], movie$y)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





###########################################
# Computing the test error by paritioning


##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0

### Data partition

set.seed(123)
V = 2
n =  NROW(movie)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) 
ii = which(id==1)
movie.train = movie[ii,]
movie.test  = movie[-ii,]


### LDA/QDA

fitldat = lda(y~., data=movie.train)
plot(fit)
fitqdat = qda(y ~., data=movie.train)


### Prediction

cutoff = 0.5
pred = predict(fitldat, newdata=movie.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(movie.test$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict(fitldat, newdata=movie.train)$posterior
pred = prediction(pred2[,2], movie.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fitqdat, newdata=movie.test)$posterior
pred = prediction(pred2[,2], movie.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC




miss.err.test = 0
cutoff = 0.5

set.seed(123)
id = sample(1:V, nrow(movie), replace = T)

for(i in 1:V) {
  
  print(i)
  
  movie.train = movie[id != i,] 
  movie.test = movie[id == i,] 
  
  fitldat2 = lda(y ~., data=movie.train)
  #fit = qda(y ~., data=german.train)
  
  pred = predict(fitldat2, newdata=movie.train)$posterior
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.train = miss.err.train + mean(movie.train$y != yhat) 
  
  pred = predict(fitldat2, newdata=movie.test)$posterior
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(movie.test$y != yhat) 
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
