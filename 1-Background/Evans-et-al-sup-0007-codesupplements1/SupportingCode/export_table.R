# Helper function to export a nicely formated table to an Excel file using the xlsx package
# Jeffrey Evans
# 2014-04-11
# 
# The xlsx package requires Java (and Microsoft Excel) to be installed on your computer (www.java.com, https://www.java.com/en/download/faq/java_win64bit.xml). The xlsx package also requries the rjava and xlsxjars packages to interface with Excel. Make sure that (a) Java is installed on your computer, (b) your Java installation "matches" your R installation, and (c) Java is on system path. By match, I mean install 64 bit Java if using 64 bit R. sessionInfo() will tell you which R you're running if you're not sure. If you don't know what the system path is or how to edit it, either ask for help from an IT professional or skip this step. It just outputs a pretty table and isn't necessary. Editing your system path can cause serious computer damage and data losses if you don't know what you're doing. Don't play with it casually. 
#
# We, the authors, assume NO RESPONSIBILITY for any damages caused by editing your system path or using this software. ***You've been warned!!*** 
# Cheers, 
# Jeff et al.
# 2014-06-27


export_table <- function(dat,filename,rowNames=F){

require(xlsx)

# create new workbook object
wb <- createWorkbook()

# create worksheet within workbook
sheet  <- createSheet(wb, sheetName="CellBlock")

#convert data to data.frame object
dat = as.data.frame(dat)

# get dimensions of data frame
ncol = dim(dat)[1]
nrow = dim(dat)[2]


# set cell style for header row with bold font and top and bottom borders
headerStyle = CellStyle(wb) + Font(wb,name="Helvetica",heightInPoints=10,isBold=T) + 
  Border(color="black", position=c("TOP", "BOTTOM"),pen=c("BORDER_DOUBLE", "BORDER_THIN")) +
  Alignment(h="ALIGN_LEFT",v="VERTICAL_TOP")

# set style for first column with left justified text
cs1 = CellStyle(wb) + Font(wb,name="Helvetica",heightInPoints=10,isBold=F) + 
  Alignment(h="ALIGN_LEFT",v="VERTICAL_TOP")

# set style for remaining columns with right justified text
cs2= CellStyle(wb) + Font(wb,name="Helvetica",heightInPoints=10,isBold=F) + 
  Alignment(h="ALIGN_RIGHT",v="VERTICAL_TOP")

colStyles <- paste('list(',toString(paste("'",1:ncol,"'",'=cs',c(1,rep(2,1,ncol-1)),sep="")),')')

addDataFrame(dat,sheet,row.names=rowNames,colnamesStyle=headerStyle,colStyle=eval(parse(text = colStyles)))

# Don't forget to save the workbook ...  
saveWorkbook(wb, filename) 

}