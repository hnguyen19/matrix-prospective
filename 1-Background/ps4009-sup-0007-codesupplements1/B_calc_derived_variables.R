## B. Create derived variables for Amaranthus epidemiology study
# run this second
# This script loads in the herbicide application dataset and calculates summary statisics on herbicde application rates, modes of action, crops, etc. It compiles them both by site and by year within site and outputs to three files in the CompiledData directory : HR_summary_by_site.csv, and HR_summary_by_siteYear.csv, and HR_summary_by_siteYearDay.csv. These will then have to be merged with the herbicide resistance data and the field attributes data to run the CART analysis.
#

# clear workspace
rm(list=ls(all=TRUE)) 

# # set the working directory to the folder containing this scrip.
# setwd("Evans_et_al_Code_Supplement")

saveOutput = T
library(data.table)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
# B.I. Data import
# B.I.a Import individual herbicide application records
dat = data.table(read.csv("SupportingData/dataframe_siteyr_corrected_JE_2014_02_18.csv",colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")),key="site.num")

# B.I.b Import site attributes
site.dat =data.table(read.csv("SupportingData/site_attributes_corrected.csv",header=T),key="site.num")

# subset out site and crop.2010 variables (this is an efficient way to subset for data.table objects)
site.dat=site.dat[,list(site.num,crop.2010)]
setkey(site.dat,site.num) #necessary to reset key variable after subsetting.


#merge them together by site.num
dat = merge(dat,site.dat,all.x=T)

# edit(dat) #take a look at the dataframe
names(dat)

# B.I.c Import herbicide attributes talbe. 
# Pull in herbicide MOA data. This will be matched to the product list in dat, which allows us to calculate statistics on MOA/year, MOA/date, etc...
# This table also includes label application rates, though we don't use these here.
herb = data.table(read.csv("SupportingData/herbicide_lookup_table.csv",colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor")))

# Set key variable
setkey(herb,product)

# examine the structure
head(herb)

str(herb)

# remove lablel source from herbicide dataset. It's there for reference, but not needed for the analysis
herb=data.table(herb[,c("Label.Source"):=NULL])

# Make sure all the herbicides in the dataset dat are represented in the herbicide data. Setdiff should have zero length.
setdiff(dat$product,herb$product)

# join the herbicide MOA table to dat
setkey(dat,product) # set the key variable to join on
dat = merge(dat,herb,all.x=T)

# convert factors to character variables for calculations
dat$product = as.character(dat$product)
dat$herb.moa1 = as.character(dat$herb.moa1)
dat$herb.moa2 = as.character(dat$herb.moa2)
dat$herb.moa3 = as.character(dat$herb.moa3)
dat$chem1 = as.character(dat$chem1)
dat$chem2 = as.character(dat$chem2)
dat$chem3 = as.character(dat$chem3)

#examine structure
str(dat)

setkey(dat,site.num,Year)

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
##### B.II. Create derived variables in a field-level dataset
# The calculations are performed in several steps to summarize the data at different hierarchical timescales. For example, daily summaries (e.g. MOA/application date) are generated for each site on each application date per year, these are then averaged or otherwise summarized per year, and finally are averaged or summarized across years to get a site-level summary statistic.

# B.II.a.1 Site level stats ignoring daily or annual strucutre
by.site = dat[,list(firstYear=min(Year), # first year of mgt data
                    missing.doy=any(is.na(unique(appl.date.doy))), # is application date missing for any applications?
                    seqYrs = length(seq(min(Year),max(Year))), # number of years expected if none in sequence are missing
                    fullSeq = length(seq(min(Year),max(Year)))==length(na.omit(unique(Year))), # number of years expected in seq
                    data.years=length(na.omit(unique(Year))), # data years per site
                    manure.site = any(manure=="yes")), # manure ever applied to field? 
              by="site.num"]

#B.II.a.2  calcualte stats by site and year
by.year <- dat[,list(missing.doy=any(is.na(unique(appl.date.doy))), # are any observations missing application date data?
                     nDays = length(unique(appl.date.doy)), # number of application dates per year
                     moa.annual=length(na.omit(unique(c(herb.moa1,herb.moa2,herb.moa3)))), # MOA/Year
                     chem.annual=length(na.omit(unique(c(chem1,chem2,chem3)))), # herbicial chemistries/Year
                     pre.applied = any(resid.herb.act=="yes"),  # was pre-emergent herbicide applied?
                     crop=na.omit(unique(crop))), # crop for site and year
               by="site.num,Year"]

#B.II.a.3  calcualte stats by site, year, and day of year (i.e. date of application). 
# For sites or applications where application date was not recorded (appl.date.doy==NA) R treats NA as a single date. We excluded
# these sites from analyses that included daily variables such as MOA/application (tank mixing).
setkey(dat,site.num,Year,appl.date.doy)
by.day <- dat[,list(crop=na.omit(unique(crop)), # crop in field during year
                    product.DOY=length(na.omit(unique(c(product)))),                   # N products applied
                    moa.DOY=length(na.omit(unique(c(herb.moa1,herb.moa2,herb.moa3)))), # N MOA applied
                    chem.DOY=length(na.omit(unique(c(chem1,chem2,chem3)))),            # N herbicidal chemistries applied
                    glyph.used=any(grepl("epsps",herb.moa1)),                          # was glyphosate applied?
                    product.names.DOY=list(sort(unique(na.omit(c(product))))),         # names of products applied
                    moa.names.DOY=list(sort(unique(na.omit(c(herb.moa1,herb.moa2,herb.moa3))))), 
                    chem.names.DOY=list(sort(unique(na.omit(c(chem1,chem2,chem3)))))),
              "site.num,Year,appl.date.doy"]
setkey(dat,site.num,Year) # set the key back to site and year

# check what data.tables are in memory and their keys
tables()


##### B.II.b Calculate seconary and tertiary summary stats (e.g. annual means of daily stats and site means of these)
#II.b.1  calculate sums and means of annual stats for each site
site.means = by.year[,list(mean.moa.per.yr=mean(moa.annual),
                           max.moa.yr=max(moa.annual),
                           prop.pre.yrs = sum(pre.applied)/length(unique(Year)),
                           prop.corn.years = sum(na.omit(crop)=="Corn")/length(na.omit(crop))),
                     by="site.num"]

#B.II.b.2  calculate sums and means of daily stats for each site-year
setkey(by.day,site.num,Year) # set key of daily stats to site and year to summarize it
day.means = by.day[,list(max.moa.tank=max(moa.DOY),
                         mean.moa.tank=mean(moa.DOY),
                         n.herb.appl=length(appl.date.doy),
                         n.gly.apps.annual = sum(glyph.used),  # calc. total gly apps/yr here instead of based on # of epsps rows
                         product.yr=length(sort(unique(na.omit(unlist(product.names.DOY))))), 
                         product.names.yr=list(sort(unique(na.omit(unlist(product.names.DOY))))), 
                         moa.names.yr=list(sort(unique(na.omit(unlist(moa.names.DOY))))), 
                         chem.names.yr=list(sort(unique(na.omit(unlist(chem.names.DOY)))))),
                   by="site.num,Year"]
setkey(day.means,"Year") # set key

#B.II.b.3 # calculate mean glyphosate applications/year from annual means of daily data.
day.to.site.means = day.means[,list(mean.gly.apps.yr=mean(n.gly.apps.annual),
                                    gly.years=sum(n.gly.apps.annual>0)),
                              by="site.num"]
setkey(day.means,site.num,Year)  # reset key variables for dat.means

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
#B.III. Herbicide turnover
#B.III.a.1 Turnover 2004-2006
# calculate herbicide beta diversity and turnover metrics as well as max run of consecutive glyphosate years.
# this is done in two different for-loops. In the first loop we calculate the metrics just for the period from 2004-2006, before GR AMATA was widespread in IL. Hence the ".B" in the variable names (for "before"). In the second loop we do the same calculataions but for all available data years from 2004-2010.
cycles.B = data.table(site.num = unique(dat$site.num),
                    p.gly.rep.B = rep(NA,length(unique(dat$site.num))),   
                    max.gly.run.B = rep(NA,length(unique(dat$site.num))),
                    betaH1.B = rep(NA,length(unique(dat$site.num))),
                    betaW.B = rep(NA,length(unique(dat$site.num))),
                    dm_turnover.B = rep(NA,length(unique(dat$site.num))),key="site.num")

# loop over sites
for (ii in 1:length(cycles.B$site.num)){

  # get data from 2004-2006 only
  siteDat = subset(dat,Year %in% 2004:2006 & site.num==cycles.B$site.num[ii])
  
  # list of years in subset for this site 
  years = unique(siteDat$Year) 
  if(length(years)>=3){ # requrire all 3 years of data
    print(c(ii,years))
    
    setkey(siteDat,"Year")
    
    # exclude ACCase herbides which only kill grasses
    siteDat$herb.moa1 = gsub("ACCase",NA,siteDat$herb.moa1,ignore.case=T)
    siteDat$herb.moa2 = gsub("ACCase",NA,siteDat$herb.moa2,ignore.case=T)
    siteDat$herb.moa3 = gsub("ACCase",NA,siteDat$herb.moa3,ignore.case=T)
    
    # make table of which herbide MOA were used each year
    siteTab = table(siteDat[,unique(c(herb.moa1,herb.moa2,herb.moa3)),by=Year])
    
    # calculate Harrison et al.'s (1992) beta-1 and Whittaker's beta diversity metrics from the table. 
    # This treats the columns (herbicides) in the table like species and the rows (years) like sites.
    cycles.B$betaH1.B[ii] = (ncol(siteTab)/mean(rowSums(siteTab)) - 1)/(dim(siteTab)[1]-1)
    cycles.B$betaW.B[ii]  = ncol(siteTab)/mean(rowSums(siteTab))
    
    # coerce to data.frame object
    df.tab = as.data.frame.matrix(siteTab)
    
    # make logical matrix showing which herbicides were used in consecutive years. 
    reps = siteTab[2:length(years),]==1 & siteTab[1:length(years)-1,]==1
    
    #\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
    # Calculate herbicide turnover index. Adapted from Diamond and May (1977, the source of the "dm" in
    # the variable name), and Wilson and Shmida (1984)'s metrics of species beta diversity and turnover.
    #
    # For each sequence of two years (i.e. each pair of adjacent rows), calculate gains and losses from
    # year 1 to year 2 and the total herbicides used across the two years. The total is scaled by the
    # number of years between samples, which downweights longer intervals. See legend of supplemental 
    # figure 3 for more details.
    gained = rowSums(siteTab[2:length(years),]==1 & siteTab[1:length(years)-1,]==0)
    lost = rowSums(siteTab[2:length(years),]==0 & siteTab[1:length(years)-1,]==1)
    Sci = rowSums(siteTab[2:length(years),]==1 | siteTab[1:length(years)-1,]==1)*
      (years[2:length(years)] - years[1:length(years)-1])
    
    cycles.B$dm_turnover.B[ii] = mean((lost+gained)/Sci)
    #\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//
    
    
    # identify which pairs of data years are consecutive. For exemple, the sequence
    # 2004,2005,2006 will return TRUE TRUE, but 2004,2006,2007 will return FALSE TRUE
    # because 2004 and 2005 are not consecutive.
    runYear = (years[2:length(years)] - years[1:length(years)-1])==1
    
    # propoprtion of consecutive years that glyphosate was applied two years in a row. 
    # for example, if it was applied in 2004 and 2005, that constitutes one repeat application.
    if ("epsps" %in% colnames(df.tab)){
      cycles.B$p.gly.rep.B[ii] = sum(df.tab$epsps[seq(2,length(years))[runYear]] &
                                     df.tab$epsps[seq(1,length(years)-1)[runYear]])/sum(runYear)
      
      # maxumum run of consecutive data years in which glyphosate was applied 
      rl = rle(df.tab$epsps) # use run lenght encoding to quantify run lengths
      cycles.B$max.gly.run.B[ii] = max(rl$lengths[rl$values==1]) # extract maximum run length 
    } else{cycles.B$p.gly.rep.B[ii] = 0 # if no glyphosate years then zero for both.
           cycles.B$max.gly.run.B[ii] = 0}  
  }
}

#B.III.a.2 merge cycles.B to by.site
by.site = merge(by.site,cycles.B,by="site.num")

#B.III.b.1 Turnover 2004-2010
# Run again, but for all years this time. See previous section for annotations.
cycles = data.table(site.num = unique(dat$site.num),
                    p.gly.rep = rep(NA,length(unique(dat$site.num))),
                    max.gly.run = rep(NA,length(unique(dat$site.num))),
                    betaH1 = rep(NA,length(unique(dat$site.num))),
                    betaW = rep(NA,length(unique(dat$site.num))),
                    dm_turnover = rep(NA,length(unique(dat$site.num))),key="site.num")

for (ii in 1:length(cycles$site.num)){
  siteDat = subset(dat,site.num==cycles$site.num[ii])
  years = unique(siteDat$Year)
  if(length(years)>=3){
    print(c(ii,years))
    setkey(siteDat,"Year")
    siteDat$herb.moa1 = gsub("ACCase",NA,siteDat$herb.moa1,ignore.case=T)
    siteDat$herb.moa2 = gsub("ACCase",NA,siteDat$herb.moa2,ignore.case=T)
    siteDat$herb.moa3 = gsub("ACCase",NA,siteDat$herb.moa3,ignore.case=T)
    
    
    siteTab = table(siteDat[,unique(c(herb.moa1,herb.moa2,herb.moa3)),by=Year])
    
    cycles$betaH1[ii] = (ncol(siteTab)/mean(rowSums(siteTab)) - 1)/(dim(siteTab)[1]-1)
    cycles$betaW[ii]  = ncol(siteTab)/mean(rowSums(siteTab))
    
    df.tab = as.data.frame.matrix(siteTab)
    
    reps = siteTab[2:length(years),]==1 & siteTab[1:length(years)-1,]==1
    
    gained = rowSums(siteTab[2:length(years),]==1 & siteTab[1:length(years)-1,]==0)
    lost = rowSums(siteTab[2:length(years),]==0 & siteTab[1:length(years)-1,]==1)
    Sci = rowSums(siteTab[2:length(years),]==1 | siteTab[1:length(years)-1,]==1)*
      (years[2:length(years)] - years[1:length(years)-1])
    
    
    cycles$dm_turnover[ii] = mean((lost+gained)/Sci)
    
    runYear = (years[2:length(years)] - years[1:length(years)-1])==1
    
     # propoprtion of consecutive years that glyphosate was applied. 
    # for example, if it was applied in 2004 and 2005, that constitutes one repeat application.
    if ("epsps" %in% colnames(df.tab)){
      cycles$p.gly.rep[ii] = sum(df.tab$epsps[seq(2,length(years))[runYear]] &
                                   df.tab$epsps[seq(1,length(years)-1)[runYear]])/sum(runYear)
      
      # maxumum run of consecutive data years in which glyphosate was applied 
      rl = rle(df.tab$epsps)
      cycles$max.gly.run[ii] = max(rl$lengths[rl$values==1])
    } else{cycles$p.gly.rep[ii] = 0
           cycles$max.gly.run[ii] = 0}  
    
    
  }
}

#B.III.b.2 merge cycles to by.site
by.site = merge(by.site,cycles,by="site.num")

##//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..//..\\..
#B.IV. Merge calcualted data tables for export
# Calculate years of data per site
nyears <- dat[,list(nyears=length(na.omit(unique(Year)))), by="site.num"]

#B.IV.a join the site-level data together
out.by.site = merge(by.site,site.means,all.x=T)
out.by.site = merge(out.by.site,day.to.site.means,all.x=T)

# calculate proportion of data years with glyphosate.
out.by.site$prop.gly.years = out.by.site$gly.years/out.by.site$data.years 

#B.IV.b join the site-year level data together
out.by.year = merge(by.year,day.means,all.x=T)

# set key variable to site so we can merge with site-level attributes
setkey(out.by.year,site.num)

# merge with site data
out.by.year = merge(out.by.year,site.dat,all.x=T) # merge in 2010 crop
out.by.year = merge(out.by.year,nyears,all.x=T)   # and number of data years

# reset keys to site and year
setkey(out.by.year,site.num,Year)

# collapse lists of MOA and chem names for export
by.day$product.names.DOY = sapply(by.day$product.names.DOY,FUN = paste,collapse = ", ")
by.day$moa.names.DOY     = sapply(by.day$moa.names.DOY,FUN = paste,collapse = ", ")
by.day$chem.names.DOY    = sapply(by.day$chem.names.DOY,FUN = paste,collapse = ", ")
out.by.year$product.names.yr = sapply(out.by.year$product.names.yr,FUN = paste,collapse = ", ")
out.by.year$moa.names.yr     = sapply(out.by.year$moa.names.yr,FUN = paste,collapse = ", ")
out.by.year$chem.names.yr    = sapply(out.by.year$chem.names.yr,FUN = paste,collapse = ", ")

#B.V. save the output
if (saveOutput==TRUE) {
  # create output directory CompiledData if it doesn't already exist.
  if (!file.exists(file.path("CompiledData"))){
    dir.create("CompiledData")
  }

  
  write.csv(out.by.site,"CompiledData/HR_summary_by_site.csv",row.names=FALSE)
  write.csv(out.by.year,"CompiledData/HR_summary_by_siteYear.csv",row.names=FALSE)
  write.csv(by.day,"CompiledData/HR_summary_by_siteYearDay.csv",row.names=FALSE)
  
}

