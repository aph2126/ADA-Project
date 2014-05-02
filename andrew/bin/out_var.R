#########################################
# Outputting the estimates 
#########################################


# Preliminaries
rm(list = ls())
setwd('/Users/howland/Dropbox/ADA Project/andrew/bin')


# Function for assigning stars for significance
stars = function(estimates) {
    
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
    return(s)
}


# Loading LASSO 
setwd("../output")
library(glmnet)
load("fit_logLASSO.Rda")
beta.logLASSO = coef(fit.logLASSO, s = "lambda.min")[-1] # Excludes repeated intercept
ix.large = order(beta.logLASSO ,decreasing=TRUE)[1:10]
names.large = (rownames(coef(fit.logLASSO, s = "lambda.min"))[-1])[ix.large]
coef.large = beta.logLASSO[ix.large]


# Loading sFWD 
load("fit_sFWD.Rda")
beta.sFWD = summary(fit.sFWD.select)$coef[,c(1,4)]
stars.sFWD = stars(beta.sFWD)
est.sFWD.out = cbind()