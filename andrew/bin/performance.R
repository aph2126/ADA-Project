

# Preliminaries
rm(list = ls())
#setwd('/home/ubuntu/andrew/bin/')
setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')
source("prelim.R")
data = prelim(server = 0, pct.test = .3, seed = 1, reduce_States = 1)
for (i in 1:length(names(data))) {
  assign(names(data)[i], data[[names(data)[i]]])
}
y.test = ad.test$TotalConversions
y.train = ad.train$TotalConversions
library(ROCR)


# Loading sSTEP results
setwd(path.output)
load("yhat_sSTEP.Rda")
pred.sSTEP = prediction(yhat.sSTEP.test, y.test)
pred.sSTEP = prediction(yhat.sSTEP.train, y.train)
auc.sSTEP = performance(pred.sSTEP, 'auc')
roc.sSTEP = performance(pred.sSTEP, "tpr", "fpr" )


# Loading sFWD results
load("yhat_sFWD.Rda")
pred.sFWD = prediction(yhat.sFWD.test, y.test)
auc.sFWD = performance(pred.sFWD, 'auc')
roc.sFWD = performance(pred.sFWD, "tpr", "fpr" )


# Loading LASSO results
setwd(path.output)
load("yhat_logLASSO.Rda")
yhat.lasso.test = yhat.test
yhat.lasso.train = yhat.train
rm(yhat.test, yhat.train)
pred.lasso = prediction(yhat.lasso.test, y.test)
auc.lasso = performance(pred.lasso, 'auc')
roc.lasso = performance(pred.lasso, "tpr", "fpr" )


# ROC Curve
y = unlist(roc.sSTEP@y.values)
x = unlist(roc.sSTEP@x.values)
y1 = unlist(roc.lasso@y.values)
x1 = unlist(roc.lasso@x.values)
y2 = unlist(roc.sFWD@y.values)
x2 = unlist(roc.sFWD@x.values)
par(mar = c(3,3,0,0) + .1)
plot(x, y, type = "l", axes = F, xlab = "", ylab = "", lwd = .5)
lines(x1, y1, type = "l", col = 2, lwd = .5)
axis(side = 1, tck = -.015, line = -.4, cex.axis = .8)
axis(side = 2, tck = -.015, line = -.4, cex.axis = .8, las = 2)
mtext(side = 1, roc.lasso@x.name, line = 2, cex = .8)
mtext(side = 2, roc.lasso@y.name, line = 2, cex = .8)


