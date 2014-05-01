
prelim = function(server = 0, pct.test = .2, seed = 1, reduce_States = 0) {

    # Working directories
    if (server == 1) {
        path.root = '/home/ubuntu/ADA-Project/'
    }
    else {
        path.root = '/Users/howland/Dropbox/ADA Project/'
    }
    path.bin = paste(path.root, 'andrew/bin/', sep = "")
    path.output = paste(path.root, 'andrew/output/', sep = "")
    path.data1 = paste(path.root, 'andrew/data/', sep = "")
    path.data2 = paste(path.root, 'data/', sep = "")
    setwd(path.bin)
    
    # Calling data
    setwd(path.data1)
    load("ad_wide.Rda")
    
    # Reducing the number of categories in State (randomForest does not like >32 levels)
    if (reduce_States == 1) {
        library(plyr)
        counts = ddply(ad, 'StateRegion', function(x) count = nrow(x))
        counts = counts[order(counts[,2], decreasing = TRUE),]
        reset.f = setdiff(counts[,1], counts[1:30,1])
        levels(ad$StateRegion)[is.element(levels(ad$StateRegion), reset.f)] = "other"
    }
    
    # Splitting into training and testing data
    set.seed(seed)
    ix.data = 1:dim(ad)[1]
    ix.test = sample(length(ix.data), size = round(length(ix.data)*pct.test))
    ad.test = ad[ix.test,]
    ad.train = ad[-ix.test,]
    return(list(ad.train = ad.train, ad.test = ad.test,
                path.bin = path.bin, path.output = path.output,
                path.data = path.data1))
}