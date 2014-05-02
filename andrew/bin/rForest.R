####################
# Random Forest
####################


# Preliminaries
rm(list = ls())
#setwd("/Users/howland/Dropbox/ADA Project/andrew/bin/")
setwd('/home/ubuntu/ADA-Project/andrew/bin/')
source("prelim.R")
data = prelim(server = 1, pct.test= .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}


# Random Forest
library(randomForest)
#library(doMC)
#registerDoMC(cores = 3)
ix.y = (names(ad.test) == "TotalConversions")
X.train = ad.train[,!ix.y]
X.test = ad.test[,!ix.y]
y.train = as.factor(ad.train[,ix.y])
y.test = as.factor(ad.test[,ix.y])


# For testing purposes
#ix.train = sample(nrow(X.train), 1000)
#ix.test = sample(nrow(X.test), 1000)
#X.train = X.train[ix.train,]
#X.test = X.test[ix.test,]
#y.train = y.train[ix.train]
#y.test = y.test[ix.test]


# Estimation 
#fit.rf <- foreach(ntree=rep(250, 9), .combine=combine,
#              .packages='randomForest') %dopar% {
#                randomForest(x = X.train, y = y.train, xtest = X.test, ytest = y.test,
#                             ntree = ntree, keep.forest = TRUE)}
fit.rForest = randomForest(x = X.train, y = y.train, xtest = X.test, ytest = y.test,
                     ntree = 3000)
yhat.rForest.train = fit.rForest$votes[,2] / rowSums(fit.rForest$votes)
yhat.rForest.test = fit.rForest$test$votes[,2] / rowSums(fit.rForest$test$votes)


# Saving fit
setwd(path.output)
save(yhat.rForest.train, yhat.rForest.test,file = "yhat_rForest.Rda")
save(fit.rForest,file = "fit_rForest.Rda")
