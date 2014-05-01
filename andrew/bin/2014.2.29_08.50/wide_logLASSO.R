####################
# Logistic LASSO
####################


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


# L1 Regularized logistic regression
library(glmnet)
X.train = model.matrix(TotalConversions ~  ., data= ad.train)
X.test = model.matrix(TotalConversions ~ - 1 + ., data= ad.test)
fit.lasso = cv.glmnet(X.train, ad.train$TotalConversions, family = "binomial", alpha = 1,)


# Logistic LASSO plots
plot(fit.lasso)
#plot(fit.lasso, xvar = "lambda", label = TRUE) need to fit glmnet()


# Selecting largest coefficients
beta.l1 = coef(fit.lasso, s = "lambda.min")[-1]     # Excludes Intercept
ix.large = order(beta.l1 ,decreasing=TRUE)[1:10]
names.large = (rownames(coef(fit.lasso, s = "lambda.min"))[-1])[ix.large]
coef.large = beta.l1[ix.large]


# Logistic LASSO fit measures
yhat.lasso.train = as.numeric(predict(fit.lasso, X.train, s = "lambda.min", type = "class"))
cRate(yhat.lasso.train, ad.train$TotalConversions)
yhat.lasso = as.numeric(predict(fit.lasso, X.test, s = "lambda.min", type = "class"))
cRate(yhat.lasso, ad.test$TotalConversions)


# Saving fit 
setwd(path.output)
save(fit.lasso,file = "long_LASSO.Rda")
