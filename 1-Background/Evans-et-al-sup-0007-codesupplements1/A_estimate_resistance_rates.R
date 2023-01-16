  ## A. Estimating herbicide resistance rates in AMATA from the raw plant counts from the greenhouse experiments.
# run this first
# This script will output the herbicide resistance estimates for each site to a file in the "CompiledData" folder called glm_resist_estimates.csv.

# The variable prop.res.plt in this file is the estimate of per per-capita resistance probability at each site. It will be merged with  other site-level data in "C_merge_site_&_HR_data.R".

# clear workspace
rm(list=ls(all=TRUE)) 

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

## load required packages
library(data.table) # for using data table class objects - a big improvement over data.frame
library(AICcmodavg) # ranks fitted models using AICc

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
#A.I. Data import
# load in raw HR survival data from the greenhouse experiments
rawDat=data.table(read.csv("SupportingData/raw_HR_data.csv"))
rawDat$round = factor(rawDat$round)
rawDat$site.num = factor(rawDat$site.num)

# Setting the key variable allows us to perform group operations, sort, and merge with other data tables. 
# It also sorts the data table by the key variable.
setkey(rawDat,site.num)

# examine structure
str(rawDat)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# A.II. Fit models
# Fit two models to greenhouse survival data. nTot is the total number of waterhemp plants exposed to glyphosate per trial; nResist is the number of survivors (i.e. resistant plants). Multiple batches of plants were tested, and plants from some sites were included in multiple batches. Round is the batch identity, which we evaluate below as a candidate random effect in m1. We also fit a generalized linear model with no random effects (m2).  

# m1 is slow to run. Uncomment to run it.
# m1 requires the lme4 package.
# library(lme4)       # fits generalized linear mixed models

# m1   = glmer(cbind(nResist,nTot-nResist)~ site.num + (1|round), data=rawDat,family="binomial",
#              control=glmerControl(optCtrl=list(maxfun=500000))) # increase maximum iterations.
m2   = glm(cbind(nResist,nTot-nResist)~ site.num, data=rawDat,family="binomial")

# extract AICc to compare models (lower = better)
# AICc(m1)
# [1] 1161.991
AICc(m2)
# [1] 1132.566

# m2 with no random effects is best supported by the data. Proceed with resistance estimates from m2.

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# A.III Gernerate predicted values
# Now predict mean resistance from best fitted model.
# Generate output table pred with list of unique sites for generating predicted resistance probabilities.

pred=data.table(site.num= unique(rawDat$site.num),
                prop.res.plt=predict(m2,newdata=data.frame(site.num= unique(rawDat$site.num)),type="response"))
setkey(pred,site.num)


# Calculate proportion of survivors per site from the raw data. Because there are no random effects in the model, the predictions from m2 are the same as these simple means per site in pResist.
setkey(rawDat,site.num)
pRaw = rawDat[,list(nResist=sum(nResist),
                    nSens=sum(nSens),
                    nTot=sum(nTot),
                    pResist=sum(nResist)/sum(nTot),
                    pSens=sum(nSens)/sum(nTot)),
              by="site.num"]

pred = merge(pred,pRaw)

# Correct for rounding error in predictions of zero resistance.
pred$prop.res.plt[pred$prop.res.plt<1e-8]=0


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# A.IV. Export results
# create output directory CompiledData if it doesn't already exist.
if (!file.exists(file.path("CompiledData"))){
  dir.create("CompiledData")
}

# export resistance estimates to file in CompliledData directory.
write.csv(pred,"CompiledData/glm_resist_estimates.csv",row.names=F)

