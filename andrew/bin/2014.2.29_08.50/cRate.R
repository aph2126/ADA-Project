######################
# Classification Rates
######################

# Function for classification rates
cRate = function(y.hat, y.test) {
    error = sum(y.hat != y.test) / length(y.hat)
    ix.0 = (y.test == 0)
    fp = sum(y.hat[ix.0] != y.test[ix.0]) / sum(ix.0)
    fn = sum(y.hat[!ix.0] != y.test[!ix.0]) / sum(!ix.0)
    return(list(error = error, fp = fp, fn = fn))
}

