var_names = function(varnames) {
    
    # Loading data
    setwd('/Users/howland/Dropbox/ADA Project/andrew/data/')
    load("ad_wide.Rda")
    varnames.new = array("", length(varnames))
    
    # Looping through variable names
    for (i in 1:length(varnames)) {
        
        # Number of ads for a certain campaign 
        if (grepl("nCamp_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nCamp_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[1]][ix.lab], ")", sep = "")
        }
    
        # Number of times visiting a site
        if (grepl("nSite_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nSite_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[2]][ix.lab], ")", sep = "")
        }
        
        # Ad type
        if (grepl("nCType_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nCType_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[3]][ix.lab], ")", sep = "")
        }
        
        # Target Group
        if (grepl("nTarg_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nTarg_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[4]][ix.lab], ")", sep = "")
        }
        
        # Ad category
        if (grepl("nCat_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nCat_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[5]][ix.lab], ")", sep = "")
        }
    
        # Ad Size
        if (grepl("nSize_", varnames[i]) == TRUE) {
            ix.lab = as.numeric(sub("nSize_", "", varnames[i]))
            varnames.new[i] = paste("#(", levels.all[[6]][ix.lab], ")", sep = "")
        }
        
        # State
        if (grepl("StateRegion", varnames[i]) == TRUE) {
            lab = sub("StateRegion", "", varnames[i])
            varnames.new[i] = paste("1(", lab, ")", sep = "")
        }
        
        # Operating system
        if (grepl("OperatingSystem", varnames[i]) == TRUE) {
            lab = sub("OperatingSystem", "", varnames[i])
            varnames.new[i] = paste("1(", lab, ")", sep = "")
        }
        
        # Browser Platform
        if (grepl("BrowserPlatform", varnames[i]) == TRUE) {
            lab = sub("BrowserPlatform", "", varnames[i])
            varnames.new[i] = paste("1(", lab, ")", sep = "")
        }
        
        # Browser Platform
        if (grepl("PlatformType", varnames[i]) == TRUE) {
            lab = sub("PlatformType", "", varnames[i])
            varnames.new[i] = paste("1(", lab, ")", sep = "")
        }
        
        # Floodlight Attribution Type
        if (grepl("FloodlightAttributionType", varnames[i]) == TRUE) {
            lab = sub("FloodlightAttributionType", "", varnames[i])
            varnames.new[i] = paste("1(", lab, ")", sep = "")
        }
    }
    ix.cont = (varnames.new == "")
    varnames.new[ix.cont] = varnames[ix.cont]
    return(varnames.new)
    
}
