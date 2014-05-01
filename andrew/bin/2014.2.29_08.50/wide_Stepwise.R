####################
# Stepwise Regression
####################


# Preliminaries
rm(list = ls())
setwd("/Users/howland/Dropbox/ADA Project/andrew/bin/")
source("prelim.R")
data = prelim(server = 0, pct.test = .8, seed = 1)
for (i in 1:length(names(data))) {
    assign(names(data)[i], data[[names(data)[i]]])
}


# Setup
library(leaps)
ad.train = ad.train[,1:12]
fit.null = glm(TotalConversions ~ 1, data = ad.train, family = "binomial")
fit.full = glm(TotalConversions ~ ., data = ad.train, family = "binomial")


# Forward Addition
fit.fwd = step (fit.null, list(lower = fit.null, upper = fit.full), direction = "forward")
vars.fwd = as.character(attr(fit.fwd$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.fwd)
fit.fwd.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")


# Backward Deletion
fit.back = step (fit.full, direction = "backward")
vars.back = as.character(attr(fit.back$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.back)
fit.back.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")


# Stepwise regression 
fit.step = step (fit.full, direction = "both")
vars.step = as.character(attr(fit.step$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.step)
fit.step.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")


# Saving results
setwd(path.output)
save(fit.fwd.select, fit.back.select, fit.step.select, file = "wide_Stepwise.Rda")


