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

set.seed(7)
V=2
n= nrow(movie)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
movie.train = movie[ii,]
movie.test  = movie[-ii,]

### logit
fitlog <- glm(y~., data=movie.train, family=binomial(link="logit"))
summary(fitlog)

# model selection
fit2 = step(fitlog, direction = "both")
fit2$anova
summary(fit2)

# Prediction
fit2.pred = predict(fit2, newdata = movie.test, type = "response") 
cutoff = 0.5
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)
ctable = table(movie.test$y, fit2.yhat,  dnn = c("Actual", "Predicted")) 
ctable

# Errors
miss.err = 1-sum(diag(ctable))/sum(ctable) ;miss.err
pred.acc = 1 - miss.err ; pred.acc

# AUC
fit.pred = predict(fitlog, newdata = movie.test, type = "response") 
pred = prediction(fit.pred, movie.test$y)
perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Logit Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
performance(pred, "auc")@y.values #AUC

