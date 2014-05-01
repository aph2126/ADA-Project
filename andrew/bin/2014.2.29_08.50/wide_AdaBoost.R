####################
# AdaBoost 
####################


# Preliminaries
rm(list = ls())
library(ada)
library(rpart)
path.root = '/Users/howland/Dropbox/ADA Project/'
path.bin = paste(path.root, 'andrew/bin/wide/', sep = "")
path.output = paste(path.root, 'andrew/output/', sep = "")
path.data1 = paste(path.root, 'andrew/data/', sep = "")
path.data2 = paste(path.root, 'data/', sep = "")
setwd(path.bin)
source("../cRate.R")


# Calling data
setwd(path.data1)
load("ad_wide.Rda")


# Splitting into training and testing data
set.seed(1)
ix.data = 1:dim(ad)[1]
ix.test = sample(length(ix.data)*.2)
ad.test = ad[ix.test,]
ad.train = ad[-ix.test,]


# AdaBoost with decision stumps
ix.y = (names(ad.test) == "TotalConversions")
stump = rpart.control(maxdepth=1, cp=-1, minsplit=0, xval=0)
fit.ada.stump = ada(x = ad.train[,!ix.y], y = ad.train[,ix.y], 
                    test.x = ad.test[,!ix.y], test.y = ad.test[,ix.y],
                    loss = "exponential", control = stump, iter = 2000)
save(fit.ada.stump, file = "wide_AdaBoost1.Rda")

fit.ada = ada(x = ad.train[,!ix.y], y = ad.train[,ix.y], 
                    test.x = ad.test[,!ix.y], test.y = ad.test[,ix.y],
                    loss = "exponential",  iter = 2000)
setwd("/Users/howland/Documents/Classes/Advanced Data Analysis/Final Project")
save(fit.ada, file = "wide_AdaBoost2.Rda")


# AdaBoost fit measures
plot(fit.ada.stump,FALSE,TRUE)
yhat.ada = predict(fit.ada.stump, ad.t est[,!ix.y])
cRate(fit.ada.stump$fit, ad.test[,ix.y])
cRate(yhat.ada, ad.test[,ix.y])


# Saving fit
setwd(path.output)
save(fit.ada, fit.ada.stump,file = "wide_AdaBoost.Rda")
