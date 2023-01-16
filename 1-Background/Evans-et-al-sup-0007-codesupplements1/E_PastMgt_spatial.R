## E. Analyses of management history (model building) and spatially oriented analyses
# The code in this script performs two sets of analyses: the generalized linear mixed model (GLMM) fitting and ranking, and the semiparital correlations. Both rely on information about the spatial relationships between fields. 


# clear workspace
rm(list=ls(all=TRUE)) 

# A few settings
saveFigs = T  # Logical switch to save output or not. Set to T or TRUE to save output, F or FALSE to save nothing.
saveExtraFigs = F  # Logical switch to save additional plots not used in manuscript.
minSites = 3  # minimum number of sites per cluster
minYears = 3  # minimum years of management data per site from 2004-2006
mdist    = 5  # minimum distance (km) to nearest neighboring field that meets other data requirements.
nclust   = 11  # number of clusters to trim tree to

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

# create output directory for figures
if (saveFigs==T | saveExtraFigs==T){
  if (!file.exists(file.path(getwd(),"Figures"))){
    dir.create("Figures")
  }
  
  if (!file.exists(file.path(getwd(),"Figures\\Model_Selection"))){
    dir.create("Figures\\Model_Selection")
  }
}

## load required packages
library(ggplot2)    # for plotting.
library(lme4)       # for fitting glmm to estimate resistance
library(gridExtra)  # for making multipanel plots with ggplot2
library(AICcmodavg) # calculates AICc from multiple object types and can return AIC tables  
library(psych)      # contains pairs.panels function for better pairs plots
library(data.table) # for using data.table class variables - improves on data.frame
library(plyr)       # for using ddply function
library(fields)     # for calculating distance matrix to get nearest infected neighbor
library(ppcor)      # for calculating partial and semipartial correlations
library(pwr)        # for power test of semipartial correlations
library(car)        # includes Anova function (with a capital "A") that performs type 3 tests.

source("SupportingCode/export_table.R") # function that exports nicely formated tables to excel.

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
#####E.I. Data 
# E.I.a. Import site level data
dat <- data.table(read.csv("CompiledData/HR_merged_by_site.csv",header=T),key="site.num")

# E.I. Drop a few sites
# exclude these fields with spotty records, with sunflowers, or without management data.
dat=subset(dat,!is.na(prop.res.plt) &mgt.data=="yes" & !(site.num %in% c(52,53,54,55,56)) & crop.2010!="Sunflower")
dat$crop.2010 = factor(dat$crop.2010) # re-define crop factor levels without sunflower
tables()

# Create binary invasion status variable
dat$invaded = as.numeric(dat$prop.res.plt>0)

# E.I.b Load derived annual GR and management data
hdat = data.table(read.csv("CompiledData/HR_summary_by_siteYear.csv",header=T),key="site.num")

# Exclude fields missing day of year data. We can't calculate tank mixes without the application dates.
# hdat = subset(hdat,missing.doy==F)

# set indexing key for hdat.
setkey(hdat,Year)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.II Compile 2004-2006 data
# Make a dataset based just on the first and last three years of data to see whether sites with high resistance now had different practices earlier.
# bdat  - for "beginning data"

# This uses the by-groups capabilities of the data.table class to do these grouped calculations quickly and efficiently.
bdat = hdat[J(2004:2006),list(missing = sum(missing.doy==F)/length(missing.doy),
                              moa.B=mean(moa.annual),
                              max.moa.tank.B=mean(max.moa.tank),
                              mean.moa.tank.B=mean(mean.moa.tank),
                              n.herb.appl.B=mean(n.herb.appl),
                              glyph.apps.B=mean(n.gly.apps.annual),
                              nyr.B = length(Year)),
            by=site.num]

# set the key variable of the new data table.
setkey(bdat,site.num)

# require at least 2 years of data from each time period
bdat = bdat[nyr.B>=minYears & missing==1,]

# merge dat to bdat.
dat = merge(dat,bdat)

#\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..
# E.III. Calculate inter-site distance matrix (km) and identify site clusters by proximity
distMat = rdist.earth(dat[,cbind(nw.lon,nw.lat)],miles = FALSE)
rownames(distMat) = dat$site.num
colnames(distMat) = dat$site.num

# Function to identify the nearest neighbor distance for each site. 
# The distance to the nearest neighbor will be returned in a new column.
minDist = function(distMat){
  distMat = as.data.table(distMat)
  idx = expand.grid(site1=1:ncol(distMat),site2=1:nrow(distMat))
  idx = subset(idx,idx$site2==idx$site1) # only keep one copy of each reciprocal pair of sites
  distMat[cbind(as.numeric(idx$site1),as.numeric(idx$site2))] = NA
  
  # this next line is really cool. it finds the minimum value within each row and puts it into a new column.
  distMat <- distMat[,minDist:=do.call(pmin, c(.SD, na.rm=TRUE)), .SDcols=names(distMat)]
  return(data.frame(distMat))
}

# run minDist function
distMat = minDist(distMat)
dat$minDist = distMat$minDist

# only retain sites whose nearest neigbor is within a specified distance (km), set with mdist. This will keep isolated sites from being used in the clustered analyses.
dat = subset(dat,minDist<=mdist)
distMat = subset(distMat,distMat$minDist<=mdist,c(distMat$minDist<=mdist & !is.na(distMat$minDist),F))
rownames(distMat) = dat$site.num
colnames(distMat) = dat$site.num


# covert to distance matrix object
d <- as.dist(distMat)

# run hierarchical clustering to identify spatial aggregations of sites
hc     <- hclust(d,method="complete")      # hierarchical clustering
plot(hc)  # plot the cluster tree


# trim clusters and merge cluster ID to dat
dat$clust <- cutree(hc,k=nclust)


# calcualte how many sites are in each cluster.
dat=merge(dat,data.table(clust = 1:max(dat$clust),nsite=as.numeric(table(dat$clust))),by="clust",all.x=T)
setkey(dat,site.num)

# Drop clusters with fewer than minimum allowable sites
distMat = distMat[which(dat$nsite>=minSites),which(dat$nsite>=minSites)]
dat = subset(dat,nsite>=minSites)


# calculate maximum distance between sites within each cluster to see how large each one is
maxDist = function(x){data.frame(maxdist = max(rdist.earth(x[,cbind("nw.lon","nw.lat")],miles = FALSE)))}
dat = merge(dat,ddply(dat,.(clust),maxDist),by="clust",all.x=T)


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.IV. Calculate pairwise differences and distances between sites
# create indexing variable idx to only keep one copy of each reciprocal pair of sites
idx = expand.grid(site1=1:ncol(distMat),site2=1:nrow(distMat))
idx = subset(idx,idx$site2>idx$site1) 
ddif = data.frame(dist = distMat[cbind(as.numeric(idx$site1),as.numeric(idx$site2))], # intersite distance
                  del.max.moa.tank.04_06 = (dat$max.moa.tank.B[idx$site1]-dat$max.moa.tank.B[idx$site2]), # delta max MOA/tank
                  del.moa.04_06 = (dat$moa.B[idx$site1]-dat$moa.B[idx$site2]),        # delta mean MOA/year
                  del.resist = (dat$prop.res.plt[idx$site1]-dat$prop.res.plt[idx$site2])) # delta glyphosate resistance


# only keep sites with 04-06 data
ddif = ddif[!is.na(ddif$del.max.moa.tank.04_06),]

# plot the pairwise differences
pairs.panels(ddif,lm=T)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.V. Semipartial correlation analysis
# Excerpt from a great primer on partial and semi-partial correlations:
# http://luna.cas.usf.edu/~mbrannic/files/regression/Partial.html
# "The partial correlation of X1 and Y controlling for X2 considers the ratio of UY: X1 to the part of Y that overlaps neither X variable, that is, UY: X1 to [Y-(Shared Y+UY: X2)]. This is because the partial removes X2 from both X1 and Y. The semipartial correlation between X1 and Y ry(1.2), however, corresponds the ratio of UY: X1to all of Y. This is because X2 is only taken from X1, not from Y."
# To evaluate the unique contribution of del.max.moa.tank.04_06 to del.resist, we can take the squared partial and squared semipartial correlations of the two variables controlling for distance.


# Semipartial correlation of del.max.moa.tank and resitance after accounting for the shared variance explanation of distance
# This is the the amount of variance in delta resistance explained uniquely by detla moa/tank (that isn't also explained by detla moa/tank).
sp.moa = spcor.test(ddif$del.resist,ddif$del.max.moa.tank.04_06,ddif$dist)
sp.moa$R2 = sp.moa$estimate^2

# Partial correlation of same. Not used in paper.
p.moa = pcor.test(ddif$del.resist,ddif$del.max.moa.tank.04_06,ddif$dist)
p.moa$R2 = p.moa$estimate^2


# Semipartial correlation of distance and resitance after accounting for the shared variance explanation of del.max.moa...
# This is the unique variance in delta resistance explained by intersite distance that isn't also explained by detla moa/tank.
sp.dist = spcor.test(ddif$del.resist,ddif$dist,ddif$del.max.moa.tank.04_06)
sp.dist$R2 = sp.dist$estimate^2

# Partial correlation of same. Not used in paper.
p.dist = pcor.test(ddif$del.resist,ddif$dist,ddif$del.max.moa.tank.04_06)
p.dist$R2 = p.dist$estimate^2

# semiparital correlations stats reported in paper.
sp.moa
sp.dist

# partial correlation stats not used.
p.moa
p.dist

# Power tests of semipartial correlations.
# Probability of type II error = 1-power.
pwr.r.test(n = sp.dist$n, r = sp.dist$estimate, sig.level = 0.05, power = NULL, alternative = c("two.sided"))
pwr.r.test(n = sp.moa$n, r = sp.moa$estimate, sig.level = 0.05, power = NULL, alternative = c("two.sided"))


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.VI. Exploratory plots of proportional resistance vs. various management predictors. Note that these don't include random cluster effects that we fit in the mixed models in the next section. 

# Set plot formatting features so we only have to enter it once.
timeTheme = theme(axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14),
                  axis.text.x  = element_text(size=12,colour="black"),
                  axis.text.y  = element_text(size=12,colour="black"),
                  strip.text.x = element_text(size=12),
                  strip.text.y = element_text(size=12),
                  legend.position="none",
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank())

dat = subset(dat,!is.na(max.moa.tank.B))


# re-do nsite
dat[,nsite:=NULL]
dat=merge(dat,data.table(clust = unique(dat$clust),nsite=as.numeric(table(dat$clust))),by="clust",all.x=T)

# make sure we only keep clusters with enough sites.
dat = subset(dat,nsite>=minSites)
dat$clust=factor(dat$clust)


# Plot GR vs. maximum MOA/tank 2004-2006
beg.max.moa.tank = ggplot(dat,aes(x=max.moa.tank.B,y=prop.res.plt,colour=factor(clust),fill=factor(clust)))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="Proportion resistant in 2010") +
  timeTheme +
  scale_x_continuous(name="Mean(Annual Max(MOA/Tank)) from 2004-2006")

# view plot
beg.max.moa.tank

# Save if desired
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/max_MOA_tank_before.jpeg", units="in", width=6,height=4,dpi=440)
}

# Split by cluster
beg.max.moa.tank + facet_wrap(~clust,ncol=4) +
  ggtitle("Split by spatial cluster")
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/max_MOA_tank_before_clust.jpeg",units="in",width=9,height=6,dpi=440)
}

# GR vs. mean MOA/tank 2004-2006
beg.mean.moa.tank = ggplot(dat,aes(x=mean.moa.tank.B,y=prop.res.plt,colour=factor(clust),fill=factor(clust)))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="Proportion resistant in 2010") +
  timeTheme +
  scale_x_continuous(name="Mean(Annual Mean(MOA/Tank)) from 2004-2006")

# view plot
beg.mean.moa.tank

# Save if desired
# This may produce a warning about fitting probabilities of 0 or 1. This is ok. It arrises from trying to fit a slope to the points in clusters that all have the same probability of resistance. I.e. you cant fit a regression to three points if X and Y are the same for all 3. 

if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_tank_before.jpeg", units="in", width=6,height=4,dpi=440)
}

# Split by cluster
beg.mean.moa.tank + facet_wrap(~clust,ncol=4) +
  ggtitle("Split by spatial cluster")
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_tank_before_clust.jpeg",units="in",width=9,height=6,dpi=440)
}

# GR vs. mean MOA/year 2004-2006
beg.moa = ggplot(dat,aes(x=moa.B,y=prop.res.plt,colour=factor(clust),fill=factor(clust)))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="Proportion resistant in 2010") +
  timeTheme +
  scale_x_continuous(name="Mean(MOA/Year) from 2004-2006")

# view plot
beg.moa

# Save if desired
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_year_before.jpeg", units="in", width=6,height=4,dpi=440)
}

# Split by cluster
beg.moa + facet_wrap(~clust,ncol=4)
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_year_before_clust.jpeg", units="in", width=9,height=6,dpi=440)
}

# GR vs. mean glyphosate applications/year 2004-2006
gly.apps = ggplot(dat,aes(x=glyph.apps.B,y=prop.res.plt,colour=factor(clust),fill=factor(clust)))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="Proportion resistant in 2010") +
  timeTheme +
  scale_x_continuous(name="Mean(MOA/Year) from 2004-2006")

# View Plot
gly.apps

# Save if desired
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_year_before.jpeg", units="in", width=6,height=4,dpi=440)
}

# Split by cluster
beg.moa + facet_wrap(~clust,ncol=4)
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_year_before_clust.jpeg", units="in", width=9,height=6,dpi=440)
}


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.VII Model fitting.

# Fit generalized linear mixed models (GLMMs) and generalized linear models for each predictor using three candidate sets of random effects:
# random intercepts per cluster
# random slopes and intercepts per cluster
# no random effects

# Then use AICc to chose which random effects structure to use. The lower the AICc, the better. See David Anderson's "Model based inference in the life sciences: a primer on evidence" (2008) for a great introduction to model selection using information theory. After chosing random effects, compare models with different fixed effects to select final best model.

# E.VII.a models with proprtional resistance as response. 
# E.VII.a.1 Fit and rank models

# E.VII.a.1.i mean of max MOA/tank per year
m.max.moa.tank.B.a = glmer(cbind(nResist,nTot-nResist)~max.moa.tank.B + (1|clust),data=dat,family="binomial")
m.max.moa.tank.B.b = glmer(cbind(nResist,nTot-nResist)~max.moa.tank.B + (max.moa.tank.B|clust),data=dat,family="binomial")
m.max.moa.tank.B.c = glm(cbind(nResist,nTot-nResist)~max.moa.tank.B,data=dat,family="binomial")

AICc(m.max.moa.tank.B.c)
aictab(list(m.max.moa.tank.B.a,m.max.moa.tank.B.b),modnames=c("m.max.moa.tank.B.a","m.max.moa.tank.B.b"))

# E.VII.a.1.ii mean of mean MOA/tank
m.mean.moa.tank.B.a = glmer(cbind(nResist,nTot-nResist)~mean.moa.tank.B + (1|clust),data=dat,family="binomial")
m.mean.moa.tank.B.b = glmer(cbind(nResist,nTot-nResist)~mean.moa.tank.B + (mean.moa.tank.B|clust),data=dat,family="binomial")
m.mean.moa.tank.B.c = glm(cbind(nResist,nTot-nResist)~mean.moa.tank.B,data=dat,family="binomial")

AICc(m.mean.moa.tank.B.c)
aictab(list(m.mean.moa.tank.B.a,m.mean.moa.tank.B.b),modnames=c("m.mean.moa.tank.B.a","m.mean.moa.tank.B.b"))

# E.VII.a.1.iii mean total MOA/year
m.moa.B.a = glmer(cbind(nResist,nTot-nResist)~moa.B + (1|clust),data=dat,family="binomial")
m.moa.B.b = glmer(cbind(nResist,nTot-nResist)~moa.B + (moa.B|clust),data=dat,family="binomial")
m.moa.B.c = glm(cbind(nResist,nTot-nResist)~moa.B,data=dat,family="binomial")

AICc(m.moa.B.c)
aictab(list(m.moa.B.a,m.moa.B.b),modnames=c("m.moa.B.a","m.moa.B.b"))

# E.VII.a.1.iv mean glyphosate apps/year
m.glyph.apps.B.a = glmer(cbind(nResist,nTot-nResist)~glyph.apps.B + (1|clust),data=dat,family="binomial")
m.glyph.apps.B.b = glmer(cbind(nResist,nTot-nResist)~glyph.apps.B + (glyph.apps.B|clust),data=dat,family="binomial")
m.glyph.apps.B.c = glm(cbind(nResist,nTot-nResist)~glyph.apps.B,data=dat,family="binomial")

AICc(m.glyph.apps.B.c)
aictab(list(m.glyph.apps.B.a,m.glyph.apps.B.b),modnames=c("m.glyph.apps.B.a","m.glyph.apps.B.b"))

# E.VII.a.1.v Harrison's beta diversity
m.betaH1.B.a = glmer(cbind(nResist,nTot-nResist)~betaH1.B + (1|clust),data=dat,family="binomial")
m.betaH1.B.b = glmer(cbind(nResist,nTot-nResist)~betaH1.B + (betaH1.B|clust),data=dat,family="binomial")
m.betaH1.B.c = glm(cbind(nResist,nTot-nResist)~betaH1.B,data=dat,family="binomial")

AICc(m.betaH1.B.c)
aictab(list(m.betaH1.B.a,m.betaH1.B.b),modnames=c("m.betaH1.B.a","m.betaH1.B.b"))

# E.VII.a.1.vi Whittaker's beta diversity
m.betaW.B.a = glmer(cbind(nResist,nTot-nResist)~betaW.B + (1|clust),data=dat,family="binomial")
m.betaW.B.b = glmer(cbind(nResist,nTot-nResist)~betaW.B + (betaW.B|clust),data=dat,family="binomial")
m.betaW.B.c = glm(cbind(nResist,nTot-nResist)~betaW.B,data=dat,family="binomial")

AICc(m.betaW.B.c)
aictab(list(m.betaW.B.a,m.betaW.B.b),modnames=c("m.betaW.B.a","m.betaW.B.b"))

# E.VII.a.1.vii herbicide turnover index
m.dm_turnover.B.a = glmer(cbind(nResist,nTot-nResist)~dm_turnover.B + (1|clust),data=dat,family="binomial")
m.dm_turnover.B.b = glmer(cbind(nResist,nTot-nResist)~dm_turnover.B + (dm_turnover.B|clust),data=dat,family="binomial")
m.dm_turnover.B.c = glm(cbind(nResist,nTot-nResist)~dm_turnover.B,data=dat,family="binomial")

AICc(m.dm_turnover.B.c)
aictab(list(m.dm_turnover.B.a,m.dm_turnover.B.b),modnames=c("m.dm_turnover.B.a","m.dm_turnover.B.b"))

# E.VII.a.1.viii propoprtion of consecutive years that glyphosate was applied
m.p.gly.rep.B.a = glmer(cbind(nResist,nTot-nResist)~p.gly.rep.B + (1|clust),data=dat,family="binomial")
m.p.gly.rep.B.b = glmer(cbind(nResist,nTot-nResist)~p.gly.rep.B + (p.gly.rep.B|clust),data=dat,family="binomial")
m.p.gly.rep.B.c = glm(cbind(nResist,nTot-nResist)~p.gly.rep.B,data=dat,family="binomial")

AICc(m.p.gly.rep.B.c)
aictab(list(m.p.gly.rep.B.a,m.p.gly.rep.B.b),modnames=c("m.p.gly.rep.B.a","m.p.gly.rep.B.b"))

# E.VII.a.2 The model structure with random slopes and intercepts per cluster was favored for almost all variables, so use that model structure.
# Compare the models using AICc
tab1 = aictab(list(m.max.moa.tank.B.b,m.mean.moa.tank.B.b,m.moa.B.b,m.glyph.apps.B.b,m.betaH1.B.b,m.betaW.B.b,m.dm_turnover.B.b,m.p.gly.rep.B.b),modnames=c("m.max.moa.tank.B.b","m.mean.moa.tank.B.b","m.moa.B.b","m.glyph.apps.B.b","m.betaH1.B.b","m.betaW.B.b","m.dm_turnover.B.b","m.p.gly.rep.B.b"))

# clean up model names in table
tab1$Modnames = gsub(".b","",tab1$Modnames)

# view the AICc table. This is table S3 from the online supplement
print(tab1)

# export table to excel if desired 
if (saveFigs==T){
  if (!file.exists(file.path(getwd(),"Tables"))){
    dir.create("Tables")
  }
  export_table(tab1,'Tables/Prop_resistant_AICc_Table.xlsx')
}

# examine the best supported model
summary(m.max.moa.tank.B.b)
Anova(m.max.moa.tank.B.b,type=3)

# Odds ratio is the exponentiated slope from the fitted regression
exp(fixef(m.max.moa.tank.B.b)[2])

# E.VII.a.3 generate model predictions for plotting
pred = data.frame(max.moa.tank.B = seq(min(dat$max.moa.tank.B),max(dat$max.moa.tank.B),length=50))
pred$pred = as.numeric(predict(m.max.moa.tank.B.b,newdata=data.frame(pred),REform=NA, type="response"))
pred$predraw = as.numeric(predict(m.max.moa.tank.B.b,newdata=data.frame(pred),REform=NA, type="link"))
pred$clust = NA


# how much more likely is a plant to be resistant at a farm with 2 MOA max vs one with 3 MOA max?

MOApred = as.numeric(predict(m.max.moa.tank.B.b,newdata=data.frame(max.moa.tank.B=2:3),REform=NA, type="response"))
(MOApred[1]/(1-MOApred[1]))/(MOApred[2]/(1-MOApred[2]))
# [1] 51.27207

# or calculte directly from the slope estimate
i = fixef(m.max.moa.tank.B.b)[2]
i2=(1/(1+exp(-(i*2))))
i3=(1/(1+exp(-(i*3))))
(i2/(1-i2))/(i3/(1-i3))

# E.VII.a.4 Plot data and marginal predictions from fitted model.
beg.max.moa.tank.fitted = 
  ggplot(data=dat,aes(x=max.moa.tank.B,y=prop.res.plt,colour=clust,fill=clust))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  theme(legend.position="none") + 
  timeTheme +
  labs(x=expression(Mean(Max~MOAs~Application^-1*~Year^-1)~from~2004-2006),
       y="Proportion resistant in 2010") +
  geom_line(data=pred,aes(x=max.moa.tank.B,y=pred,group=1),size=1,colour="black")
beg.max.moa.tank.fitted

# export to file
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/max_MOA_tank_before_fited.jpeg", units="in", width=6,height=4,dpi=800)
  ggsave(file = "Figures//Model_Selection/max_MOA_tank_before_fited.eps", units="in", width=6,height=4,dpi=800)
  ggsave(file = "Figures//Model_Selection/max_MOA_tank_before_fited.pdf", units="in", width=6,height=4,dpi=800)
}

# E.VII.b. Binary resistance. 
# This uses the same process as above using the binary presence or absence of GR A. tuberculatus per field as the response. Model seleciton ultimately favored the glm with no random effects.

# E.VII.b.1 Fit and rank models
# E.VII.b.1.i mean of max MOA/tank per year
b.max.moa.tank.B.a = glmer(invaded~max.moa.tank.B + (1|clust),data=dat,family="binomial")
b.max.moa.tank.B.b = glmer(invaded~max.moa.tank.B + (max.moa.tank.B|clust),data=dat,family="binomial")
b.max.moa.tank.B.c = glm(invaded~max.moa.tank.B,data=dat,family="binomial")

AICc(b.max.moa.tank.B.c)
aictab(list(b.max.moa.tank.B.a,b.max.moa.tank.B.b),modnames=c("b.max.moa.tank.B.a","b.max.moa.tank.B.b"))

# E.VII.b.1.ii mean of mean MOA/tank per year
b.mean.moa.tank.B.a = glmer(invaded~mean.moa.tank.B + (1|clust),data=dat,family="binomial")
b.mean.moa.tank.B.b = glmer(invaded~mean.moa.tank.B + (mean.moa.tank.B|clust),data=dat,family="binomial")
b.mean.moa.tank.B.c = glm(invaded~mean.moa.tank.B,data=dat,family="binomial")

AICc(b.mean.moa.tank.B.c)
aictab(list(b.mean.moa.tank.B.a,b.mean.moa.tank.B.b),modnames=c("b.mean.moa.tank.B.a","b.mean.moa.tank.B.b"))


# E.VII.b.1.iii mean MOA/year
b.moa.B.a = glmer(invaded~moa.B + (1|clust),data=dat,family="binomial")
b.moa.B.b = glmer(invaded~moa.B + (moa.B|clust),data=dat,family="binomial")
b.moa.B.c = glm(invaded~moa.B,data=dat,family="binomial")

AICc(b.moa.B.c)
aictab(list(b.moa.B.a,b.moa.B.b),modnames=c("b.moa.B.a","b.moa.B.b"))

# E.VII.b.1.iv mean glyphosate apps per year
b.glyph.apps.B.a = glmer(invaded~glyph.apps.B + (1|clust),data=dat,family="binomial")
b.glyph.apps.B.b = glmer(invaded~glyph.apps.B + (glyph.apps.B|clust),data=dat,family="binomial")
b.glyph.apps.B.c = glm(invaded~glyph.apps.B,data=dat,family="binomial")

AICc(b.glyph.apps.B.c)
aictab(list(b.glyph.apps.B.a,b.glyph.apps.B.b),modnames=c("b.glyph.apps.B.a","b.glyph.apps.B.b"))

# E.VII.b.1.v Harrison et al.'s (1992) beta diversity 1, labeled ??-1 in Koleff et al. (2003)
b.betaH1.B.a = glmer(invaded~betaH1.B + (1|clust),data=dat,family="binomial")
b.betaH1.B.b = glmer(invaded~betaH1.B + (betaH1.B|clust),data=dat,family="binomial")
b.betaH1.B.c = glm(invaded~betaH1.B,data=dat,family="binomial")

AICc(b.betaH1.B.c)
aictab(list(b.betaH1.B.a,b.betaH1.B.b),modnames=c("b.betaH1.B.a","b.betaH1.B.b"))

# E.VII.b.1.vi Whittaker's beta diversity
b.betaW.B.a = glmer(invaded~betaW.B + (1|clust),data=dat,family="binomial")
b.betaW.B.b = glmer(invaded~betaW.B + (betaW.B|clust),data=dat,family="binomial")
b.betaW.B.c = glm(invaded~betaW.B,data=dat,family="binomial")

AICc(b.betaW.B.c)
aictab(list(b.betaW.B.a,b.betaW.B.b),modnames=c("b.betaW.B.a","b.betaW.B.b"))

# E.VII.b.1.vii herbicide turnover index
b.dm_turnover.B.a = glmer(invaded~dm_turnover.B + (1|clust),data=dat,family="binomial")
b.dm_turnover.B.b = glmer(invaded~dm_turnover.B + (dm_turnover.B|clust),data=dat,family="binomial")
b.dm_turnover.B.c = glm(invaded~dm_turnover.B,data=dat,family="binomial")

AICc(b.dm_turnover.B.c)
aictab(list(b.dm_turnover.B.a,b.dm_turnover.B.b),modnames=c("b.dm_turnover.B.a","b.dm_turnover.B.b"))

# E.VII.b.1.viii propoprtion of consecutive years that glyphosate was applied
b.p.gly.rep.B.a = glmer(invaded~p.gly.rep.B + (1|clust),data=dat,family="binomial")
b.p.gly.rep.B.b = glmer(invaded~p.gly.rep.B + (p.gly.rep.B|clust),data=dat,family="binomial")
b.p.gly.rep.B.c = glm(invaded~p.gly.rep.B,data=dat,family="binomial")

AICc(b.p.gly.rep.B.c)
aictab(list(b.p.gly.rep.B.a,b.p.gly.rep.B.b),modnames=c("b.p.gly.rep.B.a","b.p.gly.rep.B.b"))

# E.VII.b.2. Rank models using AICc
tab2 = aictab(list(b.max.moa.tank.B.c,b.mean.moa.tank.B.c,b.moa.B.c,b.glyph.apps.B.c,b.betaH1.B.c,b.betaW.B.c,b.dm_turnover.B.c,b.p.gly.rep.B.c),modnames=c("b.max.moa.tank.B.c","b.mean.moa.tank.B.c","b.moa.B.c","b.glyph.apps.B.c","b.betaH1.B.c","b.betaW.B.c","b.dm_turnover.B.c","b.p.gly.rep.B.c"))
tab2$Modnames = gsub(".c","",tab2$Modnames)

# this is supplementary table S4.
print(tab2)

# export table to excel if desired
if (saveFigs==T){
  export_table(tab2,'Tables/Binary_resistance_AICc_Table.xlsx')
}

# Best model is mean moa/tank

# examine fitted model
summary(b.mean.moa.tank.B.c)
Anova(b.mean.moa.tank.B.c,type=3)

# Odds ratio is the exponentiated slope from the fitted regression
exp(coef(b.mean.moa.tank.B.c)[2])

# is the result robust to excluding sites with > 2.5 MOA (i.e. is that cluster of sites in Fig 2A with >2.5 MOA driving the whole regression?)?
b.mean.moa.tank.B.c2 = glm(invaded~mean.moa.tank.B,data=subset(dat,mean.moa.tank.B<2.5),family="binomial")
summary(b.mean.moa.tank.B.c2)
Anova(b.mean.moa.tank.B.c2,type=3)
# Yes, it's robust.

# E.VII.b.3. Generate marginal predicted values from best supported model 
pred$mean.moa.tank.B = seq(min(dat$mean.moa.tank.B),max(dat$mean.moa.tank.B),length=50)
pred$predb.mean.moa.tank.B.c = as.numeric(predict(b.mean.moa.tank.B.c ,newdata=data.frame(pred), type="response"))
pred$predb.mean.moa.tank.B.c2 = as.numeric(predict(b.mean.moa.tank.B.c2 ,newdata=data.frame(pred), type="response"))

# how much more likely is a plant to be resistant at a farm with 2.5 MOA max vs one with 1.5 MOA max tank moa?
idx15 = pred$mean.moa.tank.B==1.5
idx25 = pred$mean.moa.tank.B==2.5
(pred$predb.mean.moa.tank.B.c[idx15]/(1-pred$predb.mean.moa.tank.B.c[idx15]))/(pred$predb.mean.moa.tank.B.c[idx25]/(1-pred$predb.mean.moa.tank.B.c[idx25]))

# [1] 83.38421

# E.VII.b.4. plot best supported model marginal predictions with data
binary.mean.moa.tank.fitted = 
  ggplot(data=dat,aes(x=mean.moa.tank.B,y=invaded,colour=clust,fill=clust))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.02,width=0.03)) +
  theme(legend.position="none") + 
  timeTheme +
  labs(x=expression(Mean(Mean~MOAs~Application^-1*~Year^-1)~from~2004-2006),
       y="Probability of resistance in 2010") +
  geom_line(data=pred,aes(x=mean.moa.tank.B,y=predb.mean.moa.tank.B.c,group=1),size=1,colour="black") +
  geom_line(data=pred[pred$mean.moa.tank.B<2.5,],aes(x=mean.moa.tank.B,y=predb.mean.moa.tank.B.c2,group=1),size=1,linetype=4,colour="black")
binary.mean.moa.tank.fitted

#export figure
if (saveExtraFigs==TRUE){
  ggsave(file = "Figures//Model_Selection/mean_MOA_tank_before_binary_fited.jpeg", units="in", width=6,height=4,dpi=800)
  ggsave(file = "Figures//Model_Selection/mean_MOA_tank_before_binary_fited.eps", units="in", width=6,height=4,dpi=800)
  ggsave(file = "Figures//Model_Selection/mean_MOA_tank_before_binary_fited.pdf", units="in", width=6,height=4,dpi=800)
}

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# E.VIII. Export two-panel plot with proportional and binary model predictions used as figure 2
if (saveFigs==TRUE){
  
  # Save jpeg
  jpeg(file = "Figures//Model_Selection//Evans_Fig2.jpeg", units="in", width = 6, height = 8,res=800)
  grid.arrange(
    binary.mean.moa.tank.fitted + annotate("text", x = 2.7, y = 1, label = "A",size=7, fontface="bold"),
    beg.max.moa.tank.fitted + annotate("text", x = 3, y = .6, label = "B",size=7, fontface="bold"),ncol=1)  
  Sys.sleep(1)
  dev.off()

  postscript(file = "Figures//Model_Selection//Evans_Fig2.eps",horiz=FALSE,onefile=FALSE,width=6,height=8)
  grid.arrange(
    binary.mean.moa.tank.fitted + annotate("text", x = 2.7, y = 1, label = "A",size=7, fontface="bold"),
    beg.max.moa.tank.fitted + annotate("text", x = 3, y = .6, label = "B",size=7, fontface="bold"),ncol=1)  
  Sys.sleep(1)
  dev.off()

  pdf(file = "Figures//Model_Selection//Evans_Fig2.pdf", width = 6, height = 8)
  grid.arrange(
    binary.mean.moa.tank.fitted + annotate("text", x = 2.7, y = 1, label = "A",size=7, fontface="bold"),
    beg.max.moa.tank.fitted + annotate("text", x = 3, y = .6, label = "B",size=7, fontface="bold"),ncol=1)  
  Sys.sleep(1)
  dev.off()
  
  
}

