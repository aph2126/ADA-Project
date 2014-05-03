

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


# Loading predicted values, appending
setwd(path.output)
load("yhat_sSTEP.Rda")
load("yhat_logLASSO.Rda")
load("yhat_rForest.Rda")
load("yhat_svm.Rda")
yhat.svm.test = 1 - yhat.svm.test
yhat.svm.train = 1 - yhat.svm.train
yhat.train = cbind(yhat.sSTEP.train, yhat.logLASSO.train, yhat.rForest.train, yhat.svm.train)
yhat.test = cbind(yhat.sSTEP.test, yhat.logLASSO.test, yhat.rForest.test, yhat.svm.test)
colnames(yhat.train) = colnames(yhat.test) = c("Step", "LASSO", "Random Forest", "SVM")


# Performance statistics
fp.tp.train = fp.tp.test = list()
auc.train = auc.test = array(NA, ncol(yhat.train))
for (i in 1:ncol(yhat.train)) {
    pred.train.i = prediction(yhat.train[,i], y.train)
    pred.test.i = prediction(yhat.test[,i], y.test)
    perf.train.i = performance(pred.train.i,"tpr","fpr")
    perf.test.i = performance(pred.test.i,"tpr","fpr")
    auc.train[i] = unlist(performance(pred.train.i,"auc")@y.values)
    auc.test[i] = unlist(performance(pred.test.i,"auc")@y.values)
    fp.tp.train[[i]] = cbind(unlist(perf.train.i@x.values), unlist(perf.train.i@y.values))
    fp.tp.test[[i]] = cbind(unlist(perf.test.i@x.values), unlist(perf.test.i@y.values))
}


# ROC
n.cex = .7
setwd(path.output)
setwd("../paper/")
setEPS()
postscript("fig_roc.eps", height = 3, width = 6)
mat = rbind(c(1,2,3,4),c(5,6,7,8), c(9,9,9,9))
layout(mat, heights = c(.45,.55,.1), widths = c(1.1,.8,.8,.8))

par(mar = c(.5,4,0,0)+.1)
plot(x = fp.tp.train[[1]][,1], y = fp.tp.train[[1]][,2], type = "l", axes = F, ylab = "", xlab = "", col = 1)
axis(side = 1, tck = -.015, labels = NA)
axis(side = 2, tck = -.015, labels = seq(0,1,by=.25), at = seq(0,1,by=.25), las = 2, cex = n.cex)
mtext(side = 2, "TP (Train)", line = 3, cex = n.cex)
text(.75,.25, paste("AUC: ", round(auc.train[1],2), sep = ""), cex = n.cex)

par(mar = c(.5,0,0,0)+.1)
for (i in 2:ncol(yhat.train)) {
    plot(x = fp.tp.train[[i]][,1], y = fp.tp.train[[i]][,2], type = "l", axes = F, ylab = "", xlab = "", col = i)
    axis(side = 1, tck = -.015, labels = NA)
    axis(side = 2, tck = -.015, labels = NA)
    text(.75,.25, paste("AUC: ", round(auc.train[i],2), sep = ""), cex = n.cex)
}

par(mar = c(3,4,0,0)+.1)
plot(x = fp.tp.test[[1]][,1], y = fp.tp.test[[1]][,2], type = "l", axes = F, ylab = "", xlab = "", col = 1)
axis(side = 1, tck = -.015, labels = seq(0,1,by=.25), at = seq(0,1,by=.25), las = 1, cex = n.cex)
axis(side = 2, tck = -.015, labels = seq(0,1,by=.25), at = seq(0,1,by=.25), las = 2, cex = n.cex)
mtext(side = 2, "TP (Test)", line = 3, cex = n.cex)
mtext(side = 1, "FP", line = 2, cex = n.cex)
text(.75,.25, paste("AUC: ", round(auc.test[1],2), sep = ""), cex = n.cex)

par(mar = c(3,0,0,0)+.1)
for (i in 2:ncol(yhat.train)) {
    plot(x = fp.tp.test[[i]][,1], y = fp.tp.test[[i]][,2], type = "l", axes = F, ylab = "", xlab = "", col = i)
    axis(side = 1, tck = -.015, labels = seq(0,1,by=.25), at = seq(0,1,by=.25), las = 1, cex = n.cex)
    axis(side = 2, tck = -.015, labels = NA)
    mtext(side = 1, "FP", line = 2, cex = n.cex)
    text(.75,.25, paste("AUC: ", round(auc.test[i],2), sep = ""), cex = n.cex)
}

par(mar = c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,legend = c("Stepwise", "LASSO", "Random Forest", "RBF SVM"),
       col = c(1:4), lwd = 1, cex = n.cex*1.2, horiz = TRUE, bty = "n")
dev.off()

