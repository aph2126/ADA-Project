# Performace
library(ROCR)
ix.remove = (ad.test$OperatingSystem == "Windows 2000") | (ad.test$StateRegion == "NB")     # Temporary
yhat.fwd = predict(fit.fwd.select, newdata = ad.test[!ix.remove,ix], type = "response")
pred.fwd = prediction(yhat.fwd, ad.test$TotalConversions[!ix.remove])
auc.fwd = performance(pred.fwd, 'auc')
roc.fwd = performance(pred.fwd, "tpr", "fpr" )
plot(roc.fwd)
