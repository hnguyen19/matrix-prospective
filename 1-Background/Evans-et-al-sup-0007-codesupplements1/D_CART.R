# D. Run Classification and Regression Tree analyses
# clear workspace
rm(list=ls(all=TRUE)) 

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

# Logical switch to save output or not. Set to T or TRUE to save output, F or FALSE to save nothing.
saveFigs = T


## load required packages
library(data.table) # for using data.table class variables - improves on data.frame
library(rpart)      # for fitting CART models
library(rpart.plot) # for plotting superior tree figures
library(parallel)   # for running sapply in parallel on multi-core computers
library(doSNOW)     # for registering parallel interface so it's accessible by caret
library(caret)      # for bootstrapping rpart model runs
library(e1071)      # required by caret

# load local source files and functions
source("SupportingCode/my_split_fun.R") # function that adds printable labels for use in the final trees. Code provided by Stephen Millborrowm the author of rpart.plot.
source("SupportingCode/rvalues.r")  # a hack that allows multiple outputs from a function using the ":=" operator
source("SupportingCode/export_table.R") # custom function that exports nicely formated tables to excel. Examine the export_table.R file for it's requirements.


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
##D.I. Data import

# D.I.a. load in HR and field attrubute merged file.
dat <- read.csv("CompiledData/HR_merged_by_site.csv",header=T)

# D.I.b clean up formatting
dat$grass.2010 = as.logical(dat$grass.2010)
dat$border.wh.2010 = as.logical(dat$border.wh.2010)
dat$amata.absent.2010 = as.logical(dat$amata.absent.2010)
dat$amata.high.2010 = as.logical(dat$amata.high.2010)
dat$ABUTH.2010 = as.logical(dat$ABUTH.2010)
dat$AMAsp.2010 = as.logical(dat$AMAsp.2010)
dat$AMBEL.2010 = as.logical(dat$AMBEL.2010)
dat$AMBTR.2010 = as.logical(dat$AMBTR.2010)
dat$CHEAL.2010 = as.logical(dat$CHEAL.2010)
dat$ERICA.2010 = as.logical(dat$ERICA.2010)
dat$IPOHE.2010 = as.logical(dat$IPOHE.2010)
dat$SIDSP.2010 = as.logical(dat$SIDSP.2010)
dat$XANST.2010 = as.logical(dat$XANST.2010)
dat$low.spot = as.logical(dat$low.spot)
dat$watercourse.infld = as.logical(dat$watercourse.infld)
dat$watercourse.side = as.logical(dat$watercourse.side)
dat$manure = as.factor(dat$manure)


# D.I.c. Drop a few sites
# exclude these fields with spotty records.
dat=subset(dat,!(site.num %in% c(52,53,54,55,56)))

dat<-dat[dat$crop.2010!="Sunflower",] # drop one field planted with sunflower
dat$crop.2010 = factor(dat$crop.2010) # re-define crop factor levels without sunflower

#edit(dat) #take a look at the dataframe
str(dat) #view list of variable names


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.II. Variable groups for CART
# Use this section for to define the groups of independent variables to be used in the CART models. Variables are described in supplemental Fig 2.

# Soil variables
soilVars = as.character(list("P.ppm","K.ppm","Mg.ppm","Ca.ppm","pH","S.ppm","Zn.ppm","Mn.ppm","Fe.ppm",
                             "Cu.ppm","B.ppm","NO3.ppm","NH4.ppm","c.n.ratio","som.pc.comb","totN.pc",
                             "bulk.dens","sand.pc","silt.pc","clay.pc","texture","whc.0.33.bar","whc.15.bar"))
# Landscape variables
landVars = as.character(list("elev.m","max.slope","tot.border.m","p.trees.bord",
                             "p.road","closest.forest.m",
                             "closest.road.m","bare.patch.m","bare.patch.num","grs.wtwy",
                             "trans.grswtwy","grass.waterway.m","nearest.stream.m",
                             "num.fld.corners","ha","edge.interior","low.spot",
                             "watercourse.infld","watercourse.side","near.infect"))

# Weed variables 
weedVars =as.character(list("border.wh.2010","amata.infest.2010","amata.absent.2010","amata.high.2010",
                            "ABUTH.2010","AMAsp.2010","AMBEL.2010","AMBTR.2010","CHEAL.2010","ERICA.2010",
                            "grass.2010","IPOHE.2010","SIDSP.2010",
                            "XANST.2010","amata.sd.m2"))

# Management variables
mgtVars = as.character(list("manure","mean.moa.per.yr","max.moa.yr","prop.pre.yrs",
                            "mean.gly.apps.yr","prop.gly.years","dm_turnover",
                            "prop.corn.years"))

# Global variables - combind soil, weed, land and management variables. This can only be run on sites with sufficient management data.
globVars.wMgt = c(soilVars,weedVars,landVars,mgtVars)

#Global variables all - as above but without management vars. Can be run all observations.
globVars.All = c(soilVars,weedVars,landVars)


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.III. Custom plotting function for fitted rpart model. I wrote this to make it easier to repeatedly tweak the ploting features in prp.

## Custom wrapper for rpart.plot function prp.
myPRP = function(mod,modFit,modName,title="modTitle"){
  # inputs: Model object, table of model attributes (R2, titles, branch lengths), and model name (a character string that matches an entry in the modFit table)
  
  # create logical index of which lines in data frame are leaves versus node.
  # use this to determine box outline colors of leaves and nodes (nodes=white).
  cols <- ifelse(grepl("<leaf>",mod$frame$var), "black", "white")
  fonts <- ifelse(grepl("<leaf>",mod$frame$var), 3, 1)
  m.idx = modFit$modName==modName
  
  # extract R2 for main title
#   if(modFit$response[m.idx]=="Continuous"){
    if (modFit$R2[m.idx]<0.01){
      r2 = "R^2 < 0.01"
    } else {
      r2 = paste(" R^2 =",strtrim(format(modFit$R2[m.idx],digits=3),4))
    }
    if (title=="modTitle") {
      main = paste(modFit$modTitle[m.idx],r2)
    } else {
      main = paste(modFit$modTitleWrap[m.idx],r2)
    }
  
  # Plot it!
  # different plotting command for continuous vs. categorical (i.e. binary) response
  # The call to prp uses the custom function my_split_fun.R in the Supporting_Code folder to
  # change the labels used in the plots.
  if(modFit$response[m.idx]=="Continuous"){
    prp(mod, type=2, extra=1, under=T,nn=F, fallen.leaves=TRUE,
        split.fun=my_split_fun,round=0,border.col=cols,split.border.col=cols,
        faclen=0, varlen=0, branch.lty=1,branch.lwd=1,
        prefix="HR=",under.cex=.9,font=fonts,under.font=1,left=F,main=main)
  }else{
    prp(mod, type=2, extra=106, under=T,nn=F, fallen.leaves=TRUE,
        split.fun=my_split_fun,round=0,border.col=cols,split.border.col=cols,
        faclen=0, varlen=0, branch.lty=1,branch.lwd=1,
        prefix="HR=",under.cex=.9,font=fonts,under.font=1,left=F,main=main)
  }
}

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.IV. Custom wrapper for caret.
# A way of calling and bootstrappping the rpart function using the caret package.
# This custom function let me set the inputs to caret's training function and process its output without having to repeat all this code for each call to train().

my.caret <-function(xvars,yvar,c.dat,tuneLength=length(xvars)+1,repeats=30,idx=NULL,weights=NULL){
  library(rpart)
  library(caret)
  
  
#        # data for debuggging
#         yvar = dat$prop.res.plt
#         yvar = factor(dat$prop.res.plt>0)
#         xvars=c(mgtVars)
#         c.dat=dat[,xvars]
#         repeats=30
#         tuneLength=(length(xvars))-1
#         weights=dat$data.years
#         idx = dat$mgt.data=="yes" & dat$data.years>2

  
  if (is.null(idx)){
    idx = rep(T,length(yvar))
  }
  if (!is.null(weights)){
    weights = weights[idx]
  }
  
  # bind the dependent and independent variables
  c.dat = cbind(yvar,c.dat[,xvars])[idx,]
    
  # set training controls 
  # use the oneSE method described in Venables and Ripley (2002) chapter 9 to prune the final model.
  tc <- trainControl("repeatedcv", repeats=repeats, selectionFunction="oneSE",
                     returnData=FALSE,num=10)
  
  # run the model
  mod <- train(yvar ~ ., data=c.dat, method="rpart", trControl=tc,
               na.action=na.rpart,
               weights=weights,
               tuneLength=tuneLength,
               control=rpart.control(usesurrogate=0))
  
  # this almost always returns this warning:
  #   Warning message:
  #     In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
  #                               There were missing values in resampled performance measures.
  # Examining the summary statistics of the bootstrap samples model with "mode$resample" shows that the 
  # fitting sometimes returns NA for the Rsquared statistic. This is the origin of the warning.
  # However, the training routine uses RMSE to select the most parsimonious model, so this warning
  # should not be a concern. There is a discussion of this here:
  # http://stackoverflow.com/questions/10503784/caret-error-when-using-anything-but-loocv-with-rpart
  
  # plot complexity parameter vs. RMSE
  # plot(mod)

  # preview the tree
  prp(mod$finalModel,type=3,fallen.leaves=T)
  
  # look at the cptable of the final model.
  print(mod$finalModel$cptable)
  
  # get rid of spaces in the variable names
  names(mod$results) = gsub(" ","_",names(mod$results)) 
    
  # cptab = as.data.frame(pruneCaret$cptable)
  cptab = as.data.frame(mod$finalModel$cptable)
  names(cptab) = gsub(" ","_",names(cptab)) # get rid of spaces in the variable names
  
  # index best model and generate output dataset.
  best.idx = mod$results$cp==mod$bestTune$cp
  if (is.numeric(yvar)){
    out = data.frame(cp = mod$bestTune$cp,
                       R2 = mod$results$Rsquared[best.idx],
                       r2SD = mod$results$RsquaredSD[best.idx],
                       RMSE = mod$results$RMSE[best.idx],
                       RMSESD = mod$results$RMSESD[best.idx],
                       r2_2 = 1-cptab$rel_error[round(cptab$CP,digits=7)==round(mod$bestTune$cp,digits=7)],
                       nbranch = cptab$nsplit[round(cptab$CP,digits=7)==round(mod$bestTune$cp,digits=7)])
  } else{
    out = data.frame(cp = mod$bestTune$cp,
                       Accuracy = mod$results$Accuracy[best.idx],
                       AccuracySD = mod$results$AccuracySD[best.idx],
                       Kappa = mod$results$Kappa[best.idx],
                       KappaSC = mod$results$KappaSD[best.idx],
                       r2_2 = 1-cptab$rel_error[round(cptab$CP,digits=7)==round(mod$bestTune$cp,digits=7)],
                       nbranch = cptab$nsplit[round(cptab$CP,digits=7)==round(mod$bestTune$cp,digits=7)])
  }
  
  # combine output variables into a list that will return multuple outputs using :=
  answer = list(out,mod)
  print(mod$finalModel)
  print(out$r2_2)
  return(answer)
}

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.V. Run CART models

# D.V.a. Initiate parallel cluster. DoSNOW works on Windows. The documentation and website for the caret package have information on how to initiate aa parallel cluster on Mac or Linux machines.
cl <- makeCluster(getOption("cl.cores", detectCores()-1))
registerDoSNOW(cl)


# D.V.b. Create index of fields with sufficient management data for use in the management models.
idx = dat$mgt.data=="yes" & dat$data.years>2

# D.V.c. Run 'em! See comments in the my.caret function about warnings returned.
# D.V.c.1 Run proportional CART models with proportion of resistant plants per field as response.
c(mgt.Out,mgt.Mod,mgt.Fin) := my.caret(mgtVars,dat$prop.res.plt,dat,weights=dat$data.years,idx=idx)
c(glob.mgt.Out,glob.mgt.Mod,glob.mgt.Fin) := my.caret(globVars.wMgt,dat$prop.res.plt,dat,weights=dat$data.years,idx=idx)
c(soilAll.Out,soilAll.Mod,soilAll.Fin) := my.caret(soilVars,dat$prop.res.plt,dat)
c(weedAll.Out,weedAll.Mod,weedAll.Fin) := my.caret(weedVars,dat$prop.res.plt,dat)
c(landAll.Out,landAll.Mod,landAll.Fin) := my.caret(landVars,dat$prop.res.plt,dat)
c(glob.All.Out,glob.All.Mod,glob.All.Fin) := my.caret(globVars.All,dat$prop.res.plt,dat)

# D.V.c.2 Run binary CART models with presence/absence of resistance per field as response.
c(mgt.bin.Out,mgt.bin.Mod,mgt.bin.Fin) := my.caret(mgtVars,as.factor(dat$prop.res.plt>0),dat,weights=dat$data.years,idx=idx)
c(glob.mgt.bin.Out,glob.mgt.bin.Mod,glob.mgt.bin.Fin) := my.caret(globVars.wMgt,factor(dat$prop.res.plt>0),dat,weights=dat$data.years,idx=idx)
c(soilAll.bin.Out,soilAll.bin.Mod,soilAll.bin.Fin) := my.caret(soilVars,factor(dat$prop.res.plt>0),dat)
c(weedAll.bin.Out,weedAll.bin.Mod,weedAll.bin.Fin) := my.caret(weedVars,factor(dat$prop.res.plt>0),dat)
c(landAll.bin.Out,landAll.bin.Mod,landAll.bin.Fin) := my.caret(landVars,factor(dat$prop.res.plt>0),dat)
c(glob.All.bin.Out,glob.All.bin.Mod,glob.All.bin.Fin) := my.caret(globVars.All,factor(dat$prop.res.plt>0),dat)


# D.V.d. Stop the parallel cluster.
stopCluster(cl)

# D.V.e. create data frame of output from fitted CART models. This table is used in myPRP for plotting.
modFit = 
  data.frame(response=c(rep("Continuous",1,6),rep("Binary",1,6)),
             modName=c("modMgt",
                       "modGlob.mgt",
                       "modSoilAll","modLandAll","modWeedAll",
                       "modGlob.All",
                       "bin.modMgt",
                       "bin.modGlob.mgt",
                       "bin.modSoilAll","bin.modLandAll","bin.modWeedAll",
                       "bin.modGlob.All"),
             modTitle=c("Management\n",
                        "Global (Soil, Weeds, Land, Management)",
                        "Soil","Land","Weeds",
                        "Global (Soil, Weeds, Land)\n",
                        "Binary Management\n",
                        "Binary Global (Soil, Weeds, Land, Management)",
                        "Binary Soil","Binary Land","Binary Weeds",
                        "Binary Global (Soil, Weeds, Land)\n"),
             modTitleWrap=c("Management\n",
                            "Global (Soil, Weeds, Land, Management)\n",
                            "Soil","Land","Weeds",
                            "Global (Soil, Weeds, Land)\n",
                            "Binary Management\n",
                            "Binary Global (Soil, Weeds, Land, Management)\n",
                            "Binary Soil","Binary Land","Binary Weeds",
                            "Binary Global (Soil, Weeds, Land)\n"),
             nbranch = c(mgt.Out$nbranch,
                         glob.mgt.Out$nbranch,
                         soilAll.Out$nbranch,landAll.Out$nbranch,weedAll.Out$nbranch,
                         glob.All.Out$nbranch,
                         mgt.bin.Out$nbranch,
                         glob.mgt.bin.Out$nbranch,
                         soilAll.bin.Out$nbranch,landAll.bin.Out$nbranch,weedAll.bin.Out$nbranch,
                         glob.All.bin.Out$nbranch),
             R2 = c(mgt.Out$r2_2,
                    glob.mgt.Out$r2_2,
                    soilAll.Out$r2_2,landAll.Out$r2_2,weedAll.Out$r2_2,
                    glob.All.Out$r2_2,
                    mgt.bin.Out$r2_2,
                    glob.mgt.bin.Out$r2_2,
                    soilAll.bin.Out$r2_2,landAll.bin.Out$r2_2,weedAll.bin.Out$r2_2,
                    glob.All.bin.Out$r2_2),
             Accuracy = c(rep(NA,1,6),
                          mgt.bin.Out$Accuracy,
                          glob.mgt.bin.Out$Accuracy,
                          soilAll.bin.Out$Accuracy,landAll.bin.Out$Accuracy,weedAll.bin.Out$Accuracy,
                          glob.All.bin.Out$Accuracy)
  )


# Export table if desired, though not necessary. 
# Comment out if Java or xlsx cause trouble. See notes in header of export_table.R function in Supporting_Code folder.
if (saveFigs==TRUE) {
  if (!file.exists(file.path(getwd(),"Tables"))){
    dir.create("Tables")
  }
  # export table to excel
  export_table(modFit,'Tables/CART_Fit_Table.xlsx')
} 


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.VI. Plot CART trees

# D.VI.a. Plot tree models on screen.
myPRP(mgt.Mod$finalModel,modFit,"modMgt")
myPRP(glob.mgt.Mod$finalModel,modFit,"modGlob.mgt")

myPRP(soilAll.Mod$finalModel,modFit,"modSoilAll")
myPRP(landAll.Mod$finalModel,modFit,"modLandAll")
myPRP(weedAll.Mod$finalModel,modFit,"modWeedAll")
myPRP(glob.All.Mod$finalModel,modFit,"modGlob.All")

myPRP(mgt.bin.Mod$finalModel,modFit,"bin.modMgt")
myPRP(glob.mgt.bin.Mod$finalModel,modFit,"bin.modGlob.mgt")

myPRP(soilAll.bin.Mod$finalModel,modFit,"bin.modSoilAll")
myPRP(landAll.bin.Mod$finalModel,modFit,"bin.modLandAll")
myPRP(weedAll.bin.Mod$finalModel,modFit,"bin.modWeedAll")
myPRP(glob.All.bin.Mod$finalModel,modFit,"bin.modGlob.All")


##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# D.VII. Save output if desired
if (saveFigs==TRUE){

  # Save tree plots of fitted CART models
  if (!file.exists(file.path(getwd(),"Figures"))){
    dir.create("Figures")
  }
  if (!file.exists(file.path(getwd(),"Figures/CART"))){
    dir.create("Figures/CART")
  }
  
  #### Export figurs of proportional CART models (proportion resistant plants/field)
  jpeg(file = "Figures/CART//FigS1_Prop_Mgt.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(mgt.Mod$finalModel,modFit,"modMgt")
  Sys.sleep(1) 
  dev.off()
  
  postscript(file="Figures/CART//FigS1_Prop_Mgt.eps",horiz=TRUE,onefile=FALSE,width=6,height=5)
  myPRP(mgt.Mod$finalModel,modFit,"modMgt")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Prop_Glob_Mgt.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(glob.mgt.Mod$finalModel,modFit,"modGlob.mgt")
  Sys.sleep(1) 
  dev.off()
  
  ####
  jpeg(file = "Figures/CART//Prop_Soil.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(soilAll.Mod$finalModel,modFit,"modSoilAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Prop_Land.jpg", units="in", width = 5, height = 6,res=800)
  myPRP(landAll.Mod$finalModel,modFit,"modLandAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Prop_Weed.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(weedAll.Mod$finalModel,modFit,"modWeedAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Prop_Glob.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(glob.All.Mod$finalModel,modFit,"modGlob.All")
  Sys.sleep(1) 
  dev.off()
  

  #### Export figurs of binary CART models (presence/absence)
  jpeg(file = "Figures/CART//FigS2_Bin_Mgt.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(mgt.bin.Mod$finalModel,modFit,"bin.modMgt")
  Sys.sleep(1) 
  dev.off()
  
  postscript(file="Figures/CART//Figs2_Bin_Mgt.eps",horiz=TRUE,onefile=FALSE,width=6,height=5)
  myPRP(mgt.bin.Mod$finalModel,modFit,"bin.modMgt")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Bin_Glob_Mgt.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(glob.mgt.bin.Mod$finalModel,modFit,"bin.modGlob.mgt")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Bin_Soil.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(soilAll.bin.Mod$finalModel,modFit,"bin.modSoilAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Bin_Land.jpg", units="in", width = 5, height = 6,res=800)
  myPRP(landAll.bin.Mod$finalModel,modFit,"bin.modLandAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Bin_Weed.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(weedAll.bin.Mod$finalModel,modFit,"bin.modWeedAll")
  Sys.sleep(1) 
  dev.off()
  
  jpeg(file = "Figures/CART//Bin_Glob.jpg", units="in", width = 6, height = 5,res=800)
  myPRP(glob.All.bin.Mod$finalModel,modFit,"bin.modGlob.All")
  Sys.sleep(1) 
  dev.off()
}
