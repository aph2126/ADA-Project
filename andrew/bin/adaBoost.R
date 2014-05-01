####################
# AdaBoost 
####################


# Preliminaries
rm(list = ls())
#setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')
setwd('/home/ubuntu/ADA-Project/andrew/bin/')
source("prelim.R")
data = prelim(server = 1, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
  assign(names(data)[i], data[[names(data)[i]]])
}

#ix.train = sample(nrow(ad.train), 5000)
#ix.test = sample(nrow(ad.test), 500)
#ad.train = ad.train[ix.train,]
#ad.test = ad.test[ix.test,]

# AdaBoost with decision stumps
library(ada)
ix.y = (names(ad.test) == "TotalConversions")
#stump = rpart.control(maxdepth=1, cp=-1, minsplit=0, xval=0)
#fit.ada.stump = ada(x = ad.train[,!ix.y], y = ad.train[,ix.y], 
#                    test.x = ad.test[,!ix.y], test.y = ad.test[,ix.y],
#                    loss = "exponential", control = stump, iter = 3000)
#yhat.adast.test = predict(fit.ada.stump, ad.test[,!ix.y])
#yhat.adast.train = predict(fit.ada.stump, ad.train[,!ix.y])


# Saving fit
#setwd(path.output)
#save(yhat.adast.train, yhat.adast.test,file = "yhat_ADAst.Rda")
#save(fit.ada.stump, file = "fit_ADAst.Rda")


# AdaBoost with decision trees
fit.ada = ada(x = ad.train[,!ix.y], y = ad.train[,ix.y], 
                    test.x = ad.test[,!ix.y], test.y = ad.test[,ix.y],
                    loss = "exponential",  iter = 3000)
yhat.ada.train = predict(fit.ada, ad.train[,!ix.y], type = "prob")
yhat.ada.train = yhat.ada.train[,2]
yhat.ada.test = predict(fit.ada, ad.test[,!ix.y], type = "prob")
yhat.ada.test = yhat.ada.test[,2]


# Saving fit
setwd(path.output)
save(yhat.ada.train, yhat.ada.test,file = "yhat_ada.Rda")
save(fit.ada, file = "fit_ada.Rda")


# AdaBoost fit measures
#plot(fit.ada.stump,FALSE,TRUE)
