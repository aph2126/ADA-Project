####################
# Logistic LASSO
####################


# Preliminaries
rm(list = ls())
setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')
#setwd('/home/ubuntu/ADA-Project/andrew/bin/')
source("prelim.R")
data = prelim(server = 0, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
  assign(names(data)[i], data[[names(data)[i]]])
}

# For testing purposes
test = 0
if (test == 1) {
    ix.train = sample(nrow(ad.train), 1000)
    ix.test = sample(nrow(ad.test), 300)
    ad.train = ad.train[ix.train,]
    ad.test = ad.test[ix.test,]
}


# Normalizing non-factor variables
class.x = array(NA, ncol(ad.train))
for (i in 1:ncol(ad.train)) {
    class.x[i] = class(ad.train[,i])
}
ix.norm = (class.x != "factor") & (names(ad.train) != "TotalConversions")
mean = colMeans(ad.train[,ix.norm])
sd = apply(ad.train[,ix.norm], 2, sd)
ad.train[,ix.norm] = t(apply(ad.train[,ix.norm], 1, function(x) (x - mean) / sd))
ad.test[,ix.norm] = t(apply(ad.test[,ix.norm], 1, function(x) (x - mean) / sd))
ad.train[,ix.norm][!is.finite(as.matrix(ad.train[,ix.norm]))] = 0
ad.test[,ix.norm][!is.finite(as.matrix(ad.test[,ix.norm]))] = 0


# L1 Regularized logistic regression
library(glmnet)
X.train = model.matrix(TotalConversions ~  ., data= ad.train)
X.test = model.matrix(TotalConversions ~  ., data= ad.test)
fit.logLASSO = cv.glmnet(X.train, ad.train$TotalConversions, family = "binomial", alpha = 1,)
yhat.logLASSO.train = predict(fit.logLASSO, newx = X.train, s = "lambda.min", type = "response")
yhat.logLASSO.test = predict(fit.logLASSO, newx = X.test, s = "lambda.min", type = "response")


# Saving fit 
setwd(path.output)
save(yhat.logLASSO.train, yhat.logLASSO.test,file = "yhat_logLASSO.Rda")
save(fit.logLASSO,file = "fit_logLASSO.Rda")



# Selecting largest coefficients
beta.l1 = coef(fit.lasso, s = "lambda.min")[-1]     # Excludes Intercept
ix.large = order(beta.l1 ,decreasing=TRUE)[1:10]
names.large = (rownames(coef(fit.lasso, s = "lambda.min"))[-1])[ix.large]
coef.large = beta.l1[ix.large]



