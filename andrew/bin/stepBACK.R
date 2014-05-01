####################
# Stepwise Regression
####################


# Preliminaries
rm(list = ls())
#setwd('/home/ubuntu/andrew/bin/')
setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')
source("prelim.R")
data = prelim(server = 0, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}

# Temporary
ix.train = sample(nrow(ad.train), 50000)
ix.test = sample(nrow(ad.test), 10000)
ad.train = ad.train[ix.train,]
ad.test = ad.test[ix.test,]

# Setup
library(leaps)
fit.null = glm(TotalConversions ~ 1, data = ad.train, family = "binomial")
fit.full = glm(TotalConversions ~ ., data = ad.train, family = "binomial")


# Forward Addition
fit.fwd = step (fit.null, list(lower = fit.null, upper = fit.full), direction = "forward")
vars.fwd = as.character(attr(fit.fwd$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.fwd)
fit.fwd.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.train = predict(fit.fwd.select, type = "response")
yhat.test = predict(fit.fwd.select, type = "response", newdata = ad.test)


# Saving results
setwd(path.output)
save(yhat.train, yhat.test,file = "yhat_Sfwd.Rda")
save(fit.fwd.select, file = "fit_Sfwd.Rda")


# Backward Deletion
fit.back = step (fit.full, direction = "backward")
vars.back = as.character(attr(fit.back$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.back)
fit.back.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.train = predict(fit.back.select, type = "response")
yhat.test = predict(fit.back.select, type = "response", newdata = ad.test)


# Stepwise regression 
fit.step = step (fit.full, direction = "both")
vars.step = as.character(attr(fit.step$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.step)
fit.step.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.train = predict(fit.step.select, type = "response")
yhat.test = predict(fit.step.select, type = "response", newdata = ad.test)


# Saving results
setwd(path.output)
save(fit.fwd.select, fit.back.select, fit.step.select, file = "wide_Stepwise.Rda")


