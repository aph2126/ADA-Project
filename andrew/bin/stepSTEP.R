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


# Setup
library(leaps)
fit.null = glm(TotalConversions ~ 1, data = ad.train, family = "binomial")
fit.full = glm(TotalConversions ~ ., data = ad.train, family = "binomial")


# Stepwise regression 
fit.step = step(fit.full, direction = "both")
vars.step = as.character(attr(fit.step$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.step)
fit.step.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.sSTEP.train = predict(fit.step.select, type = "response")
yhat.sSTEP.test = predict(fit.step.select, type = "response", newdata = ad.test)


# Saving results
setwd(path.output)
save(yhat.sSTEP.train, yhat.sSTEP.test,file = "yhat_sSTEP.Rda")
save(fit.step.select, file = "fit_sSTEP.Rda")

