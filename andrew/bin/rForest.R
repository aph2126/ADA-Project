####################
# Random Forest
####################


# Preliminaries
rm(list = ls())
setwd("/Users/howland/Dropbox/ADA Project/andrew/bin/")
#setwd('/home/ubuntu/andrew/bin/')
source("prelim.R")
data = prelim(server = 0, pct.test= .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}


# Random Forest
library(randomForest)
library(doMC)
registerDoMC(cores = 3)
ix.y = (names(ad.test) == "TotalConversions")
X.train = ad.train[,!ix.y]
X.test = ad.test[,!ix.y]
y.train = as.factor(ad.train[,ix.y])
y.test = as.factor(ad.test[,ix.y])

ix.train = sample(nrow(X.train), 10000)
ix.test = sample(nrow(X.test), 1000)
X.train = X.train[ix.train,]
X.test = X.test[ix.test,]
y.train = y.train[ix.train]
y.test = y.test[ix.test]

ptm <- proc.time()
fit.rf <- foreach(ntree=rep(250, 9), .combine=combine,
              .packages='randomForest') %dopar% {
                randomForest(x = X.train, y = y.train, xtest = X.test, ytest = y.test,
                             ntree = ntree, keep.forest = TRUE)}
fit.rf = randomForest(x = X.train, y = y.train, xtest = X.test, ytest = y.test,
                     ntree =500, keep.forest = TRUE)
proc.time() - ptm
yhat.train = fit.rf$votes[,2] / rowSums(fit.rf$votes)
yhat.test = fit.rf$test$votes[,2] / rowSums(fit.rf$test$votes)


#
library(e1071)
fit.svm = svm(TotalConversions ~ ., data = ad.train[ix.train,], scale = F, kernel = "linear")
yhat.train = predict(fit.svm, type = "response")
yhat.test = predict(fit.svm, newdata = ad.test[ix.test,])
    
# Temp
library(ROCR)
pred = prediction(yhat.train, y.train)
pred = prediction(yhat.test, y.test)
auc = performance(pred, 'auc')
roc = performance(pred, "tpr", "fpr" )
plot(roc)




# Saving fit
setwd(path.output)
save(yhat.train, yhat.test,file = "yhat_RF.Rda")
save(fit.rf,file = "fit_RF.Rda")
