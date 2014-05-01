####################
# SVM
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

# For testing only 
#ix.train = sample(nrow(ad.train), 200)
#ix.test = sample(nrow(ad.test), 100)
#ad.train = ad.train[ix.train,]
#ad.test = ad.test[ix.test,]

# SVM
library(e1071)
#fit.svm.tune = tune(svm, as.factor(TotalConversions) ~ ., data = ad.train, probability = TRUE, scale = F, 
#              kernel = "radial", ranges = list(cost = c(.001,.01,.1,1,2)), cross = 5)
#fit.svm = fit.svm.tune$best.model
fit.svm = svm(as.factor(TotalConversions) ~ ., data = ad.train, probability = TRUE, scale = F, 
                    kernel = "radial", cost = .1)
yhat.train =  predict(fit.svm, ad.train, probability = TRUE)
yhat.svm.train = attr(yhat.train, "probabilities")[,2]
yhat.test =  predict(fit.svm, ad.test, probability = TRUE)
yhat.svm.test = attr(yhat.test, "probabilities")[,2]


# Saving fit
setwd(path.output)
save(yhat.svm.train, yhat.svm.test,file = "yhat_svm.Rda")
save(fit.svm,file = "fit_svm.Rda")
