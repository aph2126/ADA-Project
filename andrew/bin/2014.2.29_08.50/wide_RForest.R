####################
# Random Forest
####################


# Preliminaries
rm(list = ls())
setwd("/Users/howland/Dropbox/ADA Project/andrew/bin/")
source("prelim.R")
data = prelim(server = 0, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}


# Random Forest
library(randomForest)
library(doMC)
registerDoMC(cores = 16)
ix.y = (names(ad.test) == "TotalConversions")
X.train = ad.train[,!ix.y]
X.test = ad.test[,!ix.y]
y.train = as.factor(ad.train[,ix.y])
y.test = as.factor(ad.test[,ix.y])
fit.rf <- foreach(ntree=rep(500, 16), .combine=combine, .multicombine=TRUE,
              .packages='randomForest') %dopar% {
                  fit.RF = randomForest(x = X.train, y = y.train, xtest = X.test, ytest = y.test,
                                        ntree = ntree, keep.forest = TRUE)}
yhat.train = fit.rf$votes[,2] / rowSums(fit.rf$votes)
yhat.test = fit.rf$test$votes[,2] / rowSums(fit.rf$test$votes)


# Saving fit
setwd(path.output)
save(yhat.train, yhat.test,file = "yhat_RF.Rda")
save(fit.rf,file = "fit_RF.Rda")
