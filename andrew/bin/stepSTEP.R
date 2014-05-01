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

# For testing only 
#ix.train = sample(nrow(ad.train), 200)
#ix.test = sample(nrow(ad.test), 100)
#ad.train = ad.train[ix.train,]
#ad.test = ad.test[ix.test,]

# Setup
library(leaps)
fit.null = glm(TotalConversions ~ 1, data = ad.train, family = "binomial")
fit.full = glm(TotalConversions ~ ., data = ad.train, family = "binomial")


# Stepwise regression 
fit.step = step(fit.full, direction = "both")
vars.step = as.character(attr(fit.step$terms,'variables'))[-1]
ix = is.element(names(ad.train), vars.step)
fit.sSTEP.select = glm(TotalConversions ~ . , data = ad.train[,ix], family = "binomial")
yhat.sSTEP.train = predict(fit.sSTEP.select, type = "response")
yhat.sSTEP.test = predict(fit.sSTEP.select, type = "response", newdata = ad.test)


# Saving results
setwd(path.output)
save(yhat.sSTEP.train, yhat.sSTEP.test,file = "yhat_sSTEP.Rda")
save(fit.sSTEP.select, file = "fit_sSTEP.Rda")

