####################
# Stepwise Regression
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


# Setup
library(leaps)
fit.null = glm(TotalConversions ~ 1, data = ad.train, family = "binomial")
fit.full = glm(TotalConversions ~ ., data = ad.train, family = "binomial")


# Forward Addition
fit.fwd = step (fit.null, list(lower = fit.null, upper = fit.full), direction = "forward")
vars.fwd = as.character(attr(fit.fwd$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.fwd)
fit.sFWD.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.sFWD.train = predict(fit.sFWD.select, type = "response")
yhat.sFWD.test = predict(fit.sFWD.select, type = "response", newdata = ad.test)


# Saving results
setwd(path.output)
save(yhat.sFWD.train, yhat.sFWD.test,file = "yhat_sFWD.Rda")
save(fit.sFWD.select, file = "fit_sFWD.Rda")


