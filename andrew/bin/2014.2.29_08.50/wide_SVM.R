########################
# Support Vector Machine
########################


# Preliminaries
rm(list = ls())
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

# Function to create blocks for use in cross validation
blocks = function(data,K) {
    t = length(data) - (length(data) %% K)
    g = t / K
    bins = vector()
    for (k in 1:K) {
        ix = array(1,length(data))
        iz.s = (k - 1)*g + 1
        iz.e = k*g
        if ( k == K) {
            iz.e = length(data)
        }
        ix[iz.s:iz.e] = 0
        bins = cbind(bins,ix)
    }
    return(bins)
}


# Support vector machine
library(e1071)
cost = c(.1, .5, 1, 10)
gamma = .01
svm.rbf.1 = svm(as.factor(TotalConversions) ~ ., data = ad.train, kernel = 'radial', 
              cost = cost[1], gamma = gamma)
svm.rbf.1 = svm(as.factor(TotalConversions) ~ ., data = ad.train, kernel = 'radial', 
                cost = cost[1], gamma = gamma)
svm.rbf.1 = svm(as.factor(TotalConversions) ~ ., data = ad.train, kernel = 'radial', 
                cost = cost[1], gamma = gamma)
svm.rbf.1 = svm(as.factor(TotalConversions) ~ ., data = ad.train, kernel = 'radial', 
                cost = cost[1], gamma = gamma)
svm.rbf.1 = svm(as.factor(TotalConversions) ~ ., data = ad.train, kernel = 'radial', 
                cost = cost[1], gamma = gamma)
#svm.predict = predict(svm.rbf, ad.train[!ix,!ix.tc], type = "class")




