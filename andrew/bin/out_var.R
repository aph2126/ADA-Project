#########################################
# Outputting the estimates 
#########################################


# Preliminaries
rm(list = ls())
setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')


# Function for assigning stars for significance
stars = function(estimates, name = "beta") {
    
    # Assigning stars
    s = array("", nrow(estimates))
    for (i in 1:length(s)) {
        if (estimates[i,2] < .01) {
            s[i] = "***"
        } else if (estimates[i,2] < .05) {
            s[i] = "**"
        } else if (estimates[i,2] < .1) {
            s[i] = "*"
        }
    }
    
    # Outputting coefficients and stars in dataframe
    out = data.frame(estimates[,1], s)
    colnames(out) = c(name, "s")
    return(out)
}


# Loading LASSO 
setwd("../output")
library(glmnet)
load("fit_logLASSO.Rda")
beta.logLASSO = coef(fit.logLASSO, s = "lambda.min")[-2]
beta.logLASSO[beta.logLASSO == 0] = NA
beta.logLASSO = matrix(beta.logLASSO)
rownames(beta.logLASSO) = rownames(coef(fit.logLASSO, s = "lambda.min"))[-2]
colnames(beta.logLASSO) = "beta.logLASSO"

# Loading sFWD 
load("fit_sFWD.Rda")
beta.sFWD = summary(fit.sFWD.select)$coef[,c(1,4)]
stars.sFWD = stars(beta.sFWD, "beta.sFWD")


# Loading sSTEP
load("fit_sSTEP.Rda")
beta.sSTEP = summary(fit.sSTEP.select)$coef[,c(1,4)]
stars.sSTEP = stars(beta.sSTEP, "beta.sSTEP")


# Compiling estimates
out = merge(stars.sFWD, stars.sSTEP, by = 0, all = TRUE)
rownames(out) = out$Row.names
out = out[,-1]
out = merge(out, beta.logLASSO, by = 0, all = TRUE)
rownames(out) = out$Row.names
out = out[,-1]


# Outputting table
library(xtable)
n = 10
ix.top = c(order(abs(out$beta.logLASSO[-1]), decreasing = TRUE)[1:n], 1)
out[ix.top,]
