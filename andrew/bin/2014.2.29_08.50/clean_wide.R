####################
# Cleaning data
####################


# Preliminaries
rm(list = ls())
path.root = '/Users/howland/Dropbox/ADA Project/'
path.bin = paste(path.root, 'andrew/bin/', sep = "")
path.output = paste(path.root, 'andrew/output/', sep = "")
path.data1 = paste(path.root, 'andrew/data/', sep = "")
path.data2 = paste(path.root, 'data/', sep = "")


# Calling data
setwd(path.data2)
data = read.csv("cleanWide_v5.csv")


# Other
data = unique(data)                                                 # Removing duplicate rows


# Removing columns
rm.1 = c("Date", "Country", "ZIP", "City", "DMA")                   # Variables that are unrelated to conversions
rm.2 = c("TotalRevenue","Activity", "ActivityID","Row")               # Variables are identical to conversions
rm.3 = c("OperatingSystemVersion", "ConnectionType",
         "MobileCarrier", "ChannelMix", "HoursSinceAttributedAction") # Variables that are largely missing, or only have a single value
ix.remove = is.element(names(data), c(rm.1, rm.2, rm.3))
ad = data[,!ix.remove]


# Removing extraneous variables related to last 5 interactions 
vars.remove = c("CreativeGroups1", "CreativeGroups2", "PaidSearchAdGroup", "PaidSearchAdvertiser",
                "PaidSearchCampaign", "Strategy", "InteractionNumber", "InteractionDateTime",
                "Ad", "Placement", "Creative", "PaidSearchKeyword")
for (i in 1:length(vars.remove)) {
    vars.i = c(vars.remove[i], paste(vars.remove[i],"_",2:5, sep = ""))
    ix.vars = is.element(names(ad), vars.i)
    ad = ad[,!ix.vars]
}


# Collapsing variables related to last 5 interactions
vars = c("Campaign", "Site", "CreativeType", "Target", "Category", "AdSize")
names = c("Camp", "Site", "CType", "Targ", "Cat", "Size")
new.vars = matrix(NA, dim(ad)[1],0)
new.names = array(NA,0)
levels.all = list(length(vars))
for (i in 1:length(vars)) {
    
    # Identifying set of unique values for each variable 
    vars.i = c(vars[i], paste(vars[i],"_",2:5, sep = ""))
    ix.vars = is.element(names(ad), vars.i)
    levels = array(NA,0)
    for (j in 1:sum(ix.vars)) {
        levels = c(levels, levels(ad[,ix.vars][,j]))
    }
    levels = unique(levels)
    levels = matrix(levels[levels != ""])
    colnames(levels) = vars[i]
    levels.all[[i]] = levels
    
    # For each level of each variable, creating occurance counts
    for (j in 1:length(levels)) {
        name.i = paste("n", names[i],"_", j, sep = "")
        new.vars = cbind(new.vars, apply(ad, 1, function(x) sum(is.element(t(x[ix.vars]), levels[j]))))
        new.names = c(new.names, name.i)
    }
    
    # Removing original variables
    ad = ad[,!ix.vars]
}


# Cleaning up new variable names, adding in new data
new.names = gsub("-", "", new.names)
new.names = gsub(" ", "", new.names)
colnames(new.vars) = new.names
ad = cbind(ad, new.vars)


# Converting time variables
convert.time = c("ActivityDateTime")
ix.convert = is.element(names(ad), convert.time)
ad$ActivityDateTime = as.POSIXct(ad$ActivityDateTime)


# Saving data
setwd(path.data1)
save(ad,levels.all, file="ad_wide.Rda")
