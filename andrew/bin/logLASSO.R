####################
# Logistic LASSO
####################


# Preliminaries
rm(list = ls())
setwd('/home/ubuntu/andrew/bin/')
#setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')
source("prelim.R")
data = prelim(server = 1, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}

#ix.train = sample(nrow(ad.train), 1000)
#ix.test = sample(nrow(ad.test), 300)
#ad.train = ad.train[ix.train,]
#ad.test = ad.test[ix.test,]

# L1 Regularized logistic regression
library(glmnet)
X.train = model.matrix(TotalConversions ~  ., data= ad.train)
X.test = model.matrix(TotalConversions ~  ., data= ad.test)
fit.lasso = cv.glmnet(X.train, ad.train$TotalConversions, family = "binomial", alpha = 1,)
yhat.train = predict(fit.lasso, newx = X.train, s = "lambda.min", type = "response")
yhat.test = predict(fit.lasso, newx = X.test, s = "lambda.min", type = "response")


# Saving fit 
setwd(path.output)
save(yhat.train, yhat.test,file = "yhat_logLASSO.Rda")
save(fit.lasso,file = "fit_LASSO.Rda")



# Selecting largest coefficients
beta.l1 = coef(fit.lasso, s = "lambda.min")[-1]     # Excludes Intercept
ix.large = order(beta.l1 ,decreasing=TRUE)[1:10]
names.large = (rownames(coef(fit.lasso, s = "lambda.min"))[-1])[ix.large]
coef.large = beta.l1[ix.large]



