## F. Analysis and plots of herbicide beta diversity and turnover

# clear workspace
rm(list=ls(all=TRUE)) 

saveFigs = T
minYears = 3 # minimum years of management data per site from 2004-2006

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

# create output directory for figures and tables
if (saveFigs==T){
  if (!file.exists(file.path(getwd(),"Figures"))){
    dir.create("Figures")
  }
  
  if (!file.exists(file.path(getwd(),"Figures\\Herb_Diversity"))){
    dir.create("Figures\\Herb_Diversity")
  }
  
  if (!file.exists(file.path(getwd(),"Tables"))){
    dir.create("Tables")
  }
  
}

## load required packages
library(ggplot2)    # for plotting nice figures
library(car)        # for calling Anova function
library(AICcmodavg) # calculates AICc from multiple object types and can return AIC tables  
library(psych)      # contains pairs.panels function for better pairs plots
library(data.table) # for using data.table class variables - improves on data.frame
library(gridExtra)  # for making multi-panel figures with ggplot2

source("SupportingCode/my_split_fun.R") # function that substitues nicer variable labels for plotting
source("SupportingCode/export_table.R") # function that exports nicely formated tables to excel. See notes in export_table.R regarding its use.

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


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
#####F.I. Data import
# F.I.a. Load site-level GR and management summaries
dat <- data.table(read.csv("CompiledData/HR_merged_by_site.csv",header=T),key="site.num")

# exclude fields from this one farmer with spotty records.
# just the fields with two or more years of management data
dat=subset(dat,mgt.data=="yes" & !(site.num %in% c(52,53,54,55,56)) & crop.2010!="Sunflower")
dat$crop.2010 = factor(dat$crop.2010)

# require at least 3 years of management data per site
dat=subset(dat,data.years>=3)


# F.I.b. Load and process annual derived variables.
#F.I.b.1 Load derived annual HR data
hdat = data.table(read.csv("CompiledData/HR_summary_by_siteYear.csv",header=T),key="site.num")

# requre application date data so we can calculate tank mixes
hdat = subset(hdat,missing.doy==F)

# set indexing key for hdat.
setkey(hdat,Year)

# F.I.b.2 
# Make a dataset based just on the first three years of data to see whether herbicide rotation or tank mixing in these years affect 2010 glyphosate resistance.
# This step was run in E_PastMgt_spatial.R as well. 
# bdat  - for "beginning data"
bdat = hdat[J(2004:2006),list(moa.B=mean(moa.annual),
                              max.moa.tank.B=mean(max.moa.tank),
                              mean.moa.tank.B=mean(mean.moa.tank),
                              n.herb.appl.B=mean(n.herb.appl),
                              glyph.apps.B=mean(n.gly.apps.annual),
                              nyr.B = length(Year)), # number of years of data during this period.
            by=site.num]

setkey(bdat,site.num)

# require all 3 years of data from this period
bdat = bdat[nyr.B>=3,]

# merge dat to bdat.
dat = merge(dat,bdat,all.x=T)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# F.II. Plot glyphosate resistance probabiliyt vs. herbicide diversity and turnover metrics.

# F.II.a. Harrison's beta diversity (2004-2010)
betaDivH1.B = ggplot(dat,aes(x=betaH1.B,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name=expression(beta[H1]~2004-2006))

# View plot
betaDivH1.B

# F.II.b. Harrison's beta diversity (all years)
betaDivH1 = ggplot(dat,aes(x=betaH1,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name=expression(beta[H1]))

# View plot
betaDivH1

# F.II.c. Herbicide turnover index (2004-2006)
turnover.B = ggplot(dat,aes(x=dm_turnover.B,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name="Herbicide Turnover Index 2004-2006")

# View plot
turnover.B

# F.II.d. Herbicide turnover index (all years)
turnover = ggplot(dat,aes(x=dm_turnover,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name="Herbicide Turnover Index")

# View plot
turnover

# F.II.e. Whittaker's beta diversity (2004-2006)
whittaker.B = ggplot(dat,aes(x=betaW.B,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name=expression(beta[Whittaker]~2004-2006))

# View plot
whittaker.B

# F.II.d. Whittaker's beta diversity (all years)
whittaker = ggplot(dat,aes(x=betaW,y=prop.res.plt,colour='black',fill="black"))+
  geom_jitter(shape=10, size=5,position = position_jitter(height=0.01,width=0.01)) +
  stat_smooth(method="glm",family="binomial",size=1, se=FALSE,aes(weight=nTot,group=1),colour="black")+
  theme(legend.position="none") + 
  scale_y_continuous(name="P[resistant 2010]") +
  timeTheme +
  scale_x_continuous(name=expression(beta[Whittaker]))

# View plot
whittaker

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# F.III. Arrange multi-panel figure with plots of 2004-2006 data on left, and plots of all years on the right
grid.arrange(betaDivH1.B,betaDivH1,whittaker.B,whittaker,turnover.B,turnover,ncol=2)

# save figure if desired.
if (saveFigs==TRUE){
  # as jpeg
  jpeg(file = "Figures//Herb_Diversity//Evans_Fig_S4_betaDiversity.jpeg", units="in", width=8,height=10,res=800)
  grid.arrange(betaDivH1.B,betaDivH1,whittaker.B,whittaker,turnover.B,turnover,ncol=2)
  Sys.sleep(1)
  dev.off()
  
  # as eps
  postscript(file = "Figures//Herb_Diversity//Evans_Fig_S4_betaDiversity.eps", width=8,height=10)
  grid.arrange(betaDivH1.B,betaDivH1,whittaker.B,whittaker,turnover.B,turnover,ncol=2)
  Sys.sleep(1)
  dev.off()
  
  # as pdf
  pdf(file = "Figures//Herb_Diversity//Evans_Fig_S4_betaDiversity.pdf", width=8,height=10)
  grid.arrange(betaDivH1.B,betaDivH1,whittaker.B,whittaker,turnover.B,turnover,ncol=2)
  Sys.sleep(1)
  dev.off()
}

# examine relationships between variables
pairs.panels(subset(dat,,c("betaH1","betaW","dm_turnover","prop.res.plt")),lm=T)


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# F.IV. Post-hoc logistic regressions of resistance vs. each variable. We can't create equivalent clusters of sites for both time periods since the "all years" group has more sites than the 2004-2006 group. Because of this, just fit basic logistic regressions and rank models within each time period.

# F.IV.a Fit models to proportional resistance
betaH1.B = glm(cbind(nResist,nTot-nResist)~betaH1.B,data=dat,family="binomial")      # Harrison's beta
betaW.B = glm(cbind(nResist,nTot-nResist)~betaW.B,data=dat,family="binomial")        # Whittaker's beta 
betaT.B = glm(cbind(nResist,nTot-nResist)~dm_turnover.B,data=dat,family="binomial")  # herbicide turnover index
moa.yr.B = glm(cbind(nResist,nTot-nResist)~moa.B,data=dat,family="binomial")         # mean toal MOA/year
max.moa.B = glm(cbind(nResist,nTot-nResist)~max.moa.tank.B,data=dat,family="binomial") # mean (max (MOA/tank)/year)

# F.IV.b Fit models to binary resistance
betaH1 = glm(cbind(nResist,nTot-nResist)~betaH1,data=dat,family="binomial")          # Harrison's beta
betaW = glm(cbind(nResist,nTot-nResist)~betaW,data=dat,family="binomial")            # Whittaker's beta
betaT = glm(cbind(nResist,nTot-nResist)~dm_turnover,data=dat,family="binomial")      # herbicide turnover index
moa.yr = glm(cbind(nResist,nTot-nResist)~mean.moa.per.yr,data=dat,family="binomial") # mean total MOA/year

# F.IV.c. Rank models using AICc
# a casual look at the model rankings for the GLMs fit to 2004-2006 data 
aictab(list(betaH1.B,betaW.B,betaT.B,max.moa.B,moa.yr.B),modnames=c("betaH1.B","betaW.B","betaT.B","max.moa.B","moa.yr.B"))
# Max MOA/tank is clearly the best supported model. Better than any cycling index.

# Same method for the models fit to all available years of data
aictab(list(betaH1,betaW,betaT,moa.yr),modnames=c("betaH1","betaW","betaT","moa.yr"))
# in this case, the model of herbicide turnover is clearly the best, though it has a positive slope, meaning that higher turnover is associated with greater resistance rates.

# F.IV.d. Examine at the fitted models
summary(betaH1.B)
summary(betaW.B)
summary(betaT.B)
summary(moa.yr.B)
summary(max.moa.B)
summary(betaH1)
summary(betaW)
summary(betaT)
summary(moa.yr)

Anova(max.moa.B,type=3) # the slope for the tank mixing model (2004-2006) is significant 
Anova(betaT,type=3) # the slope for the turover model (2004-2010) is significant 

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# F.V. Export tables
# F.V.a. extract coefficent tables
c_betaH1.B = data.frame(Variable=row.names(coef(summary(betaH1.B))),coef(summary(betaH1.B)))
c_betaW.B = data.frame(Variable=row.names(coef(summary(betaW.B))),coef(summary(betaW.B)))
c_betaT.B = data.frame(Variable=row.names(coef(summary(betaT.B))),coef(summary(betaT.B)))
c_betaH1 = data.frame(Variable=row.names(coef(summary(betaH1))),coef(summary(betaH1)))
c_betaW = data.frame(Variable=row.names(coef(summary(betaW))),coef(summary(betaW)))
c_betaT = data.frame(Variable=row.names(coef(summary(betaT))),coef(summary(betaT)))

# F.V.b. substitute nice variable names for exported tables using custom function
c_betaH1.B$Variable = my_split_fun(x=NULL,c_betaH1.B$Variable, digits=NULL, varlen=NULL, faclen=NULL)
c_betaW.B$Variable = my_split_fun(x=NULL,c_betaW.B$Variable, digits=NULL, varlen=NULL, faclen=NULL)
c_betaT.B$Variable = my_split_fun(x=NULL,c_betaT.B$Variable, digits=NULL, varlen=NULL, faclen=NULL)
c_betaH1$Variable = my_split_fun(x=NULL,c_betaH1$Variable, digits=NULL, varlen=NULL, faclen=NULL)
c_betaW$Variable = my_split_fun(x=NULL,c_betaW$Variable, digits=NULL, varlen=NULL, faclen=NULL)
c_betaT$Variable = my_split_fun(x=NULL,c_betaT$Variable, digits=NULL, varlen=NULL, faclen=NULL)


# F.V.c. export to files if desired.
if (saveFigs==T){
  export_table(c_betaH1.B,'Tables/mod_betaH1.B.xlsx')
  export_table(c_betaW.B,'Tables/mod_betaW.B.xlsx')
  export_table(c_betaT.B,'Tables/mod_betaT.B.xlsx')
  export_table(c_betaH1,'Tables/mod_betaH1.xlsx')
  export_table(c_betaW,'Tables/mod_betaW.xlsx')
  export_table(c_betaT,'Tables/mod_betaT.xlsx')
}

