## C. Creating derived variables for AMATA epidemiology study
# This script loads in the derived herbicide application data and mereges it with the herbicide resistance data and the field attributes data.

# clear workspace
rm(list=ls(all=TRUE)) 

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

saveOutput = TRUE

# load required packages
library(data.table) # for using data.table class
library(fields)     # for calculating distance matrix to get nearest infected neighbor

#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..
# C.I. Data import
# Import herbicide resistance rates
res.dat <- data.table(read.csv("CompiledData/glm_resist_estimates.csv",header=T))
setkey(res.dat,"site.num")

# Import site attribute data
site.dat =data.table(read.csv("SupportingData/site_attributes_corrected.csv",header=T))
setkey(site.dat,"site.num")

# Import site-level derived herbicide use statistics
herb.dat = data.table(read.csv("CompiledData/HR_summary_by_site.csv"))
setkey(herb.dat,"site.num")


#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..
# C.II. Merge site data to resistance data
dat = merge(res.dat,site.dat,all.x=T)

#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..
# C.III. Calculate distance (km) from each site to nearest infected neighbor site.
distMatrix <- rdist.earth(dat[dat$prop.res.plt>0 ,cbind(nw.lon,nw.lat)],
                          dat[,cbind(nw.lon,nw.lat)],
                          miles = FALSE)
rnames = dat$site.num[dat$prop.res.plt>0]
cnames = dat$site.num
dat$near.infect = 0

# now find the nearest site with a different site name
for (ii in 1:nrow(dat)){
  dat$near.infect[ii] = min(distMatrix[rnames!=(cnames)[ii],ii])
}

#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..
# C.IV. Next merge derived variables
dat = merge(dat,herb.dat,all.x=T)

# If either manure variable was TRUE, make true. Otherwise, false
dat$manure = as.logical(dat$manure)
dat$manure = dat$manure|dat$manure.site
dat$manure[is.na(dat$manure)] = F

dat = data.table(dat[,c("manure.site"):=NULL]) # drop second manure variable
#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//.. 
# C.V. Export merged datasets
# create output directory CompiledData if it doesn't already exist.
if (saveOutput == TRUE){
  if (!file.exists(file.path(getwd(),"CompiledData"))){
    dir.create("CompiledData")
  }
  write.csv(dat,"CompiledData/HR_merged_by_site.csv",row.names=F)
}