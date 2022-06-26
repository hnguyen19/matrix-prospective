Code and data supplement for Evans et al. Temporal structure of selection pressures regulates herbicide resistance evolution.

The computer code and data files included in this supplement execute all analyses and create all figures and tables presented in the paper except the maps in Figures 1 and S1. The code is extensively annotated and should be accessible to users familiar with the R programming language. To run the code, first unzip the compressed .zip folder you downloaded to your computer. If you are reading this, you've probably already done so. I 

The analyses were conducted using 64 bit R Version 3.0.2 with RStudio Version 0.98.953 as the front end on a computer running Windows 7 Pro 64 bit. There is no guarantee that they will run correctly using any other version of R, though they’ll probably run on 32 bit R version 3.0.2. I ran into trouble trying to run them on version 3.1.1. Who knows if they'll work on R ten years from now? 

For this reason, I'm providing some very explicit instructions on how to set up your compter to run the analyses. You need the right version of R, you need to set up R so it can build packages from source code, and you need a package manager called packrat to ensure that R uses the right versions of the required packages. It's a bit tedious, but it should ensure your success. 

**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//
The supplement contains the R scripts  that run the analyses plus supporting code, supporting data, an R project file that unifies them, and a packrat directory that helps manage the add-on packages necessary to run the analyses.


To run the code:
1. Install R Version 3.0.2
	You can download the correct version of R here:

	Windows: http://cran.r-project.org/bin/windows/base/old/3.0.2

	Mac: I am not a Mac user, but I believe this is the intaller:
	http://cran.r-project.org/bin/macosx/old/R-3.0.2.pkg

	Linux: If you’re nerdly enough to be using Linux, I’m sure you can find the installer on your own.

2. Install RStudio Version 0.98.953 (recommended, though maybe not needed):
            	  Windows: http://download1.rstudio.org/RStudio-0.98.953.exe
	          Mac    : http://download1.rstudio.org/RStudio-0.98.953.dmg
        Debian 6+/Ubuntu : http://download1.rstudio.org/RStudio-0.98.953-amd64.deb
	Fedora 10+/openSUSE 11.4+: http://download1.rstudio.org/rstudio-0.98.953-x86_64.rpm
3. Open R Version 3.0.2
	- in Windows hold control key while clicking on the R or RStudio icon
	- in the pop-up window that appears, select R Version 3.0.2.
4. Your computer needs to be set up to build R packages from source code.
	On a Windows machine you'll need to install Rtools and make sure it's on your system path.

	This article on the Rstudio site explains how to do this.
	https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites
	
	Further instructions are here:
	http://cran.r-project.org/bin/windows/Rtools/

	For Windows Users:	
	4a) Download this file:
	    http://cran.r-project.org/bin/windows/Rtools/Rtools31.exe

	4b) Run the installer.
            Choose the options to install the R toolset, Cygwin DLLs, and R 2.15+ toolchain.
	    On the next screen, choose the option to let it edit your PATH so R knows how to find it.
	    Accept the PATH edits it suggests.
	    Install it. 


5. Install the R delvopment tools and packrat packages. Paste this code into the R console and hit enter:
	install.packages("devtools")
	install.packages("packrat")
   These will allow you to easily install the correct versions of all dependent packages.
6. Quit R.
7. Open the project directory Evans_et_al_Code_Supplement
8. Open the project file Evans_et_al_Code_Supplement.Rproj in R Version 3.0.2.
	- in Windows hold control key while clicking on the file icon
	- in the pop-up window that appears, select R Version 3.0.2.
   Packrat should download and install the packages required to run the analyses from the CRAN-R repository into a private package library in the packrat/lib directory of Evans_et_al_Code_Supplement. Now you're ready to go!!

9. Run the R code from A-F. See detailed descriptions of the individual scripts below.

The code is split into six R scripts titled A-F. These are intended to be executed sequentially, as outputs from earlier scripts are required as input to later ones. Briefly, the seven scripts and their purposes are:

A_estimate_resistance_rates.R - Loads raw survival data from greenhouse glyphosate exposure experiment and calculates glyphposate-resistance rate for each site (field). Output is saved to a new folder "CompiledData".

B_calc_derived_variables.R - Loads raw management data and calculates derived management variables at daily, annual, and study-wide timescales for each site. Output saved to "CompiledData" directory.

C_merge_site_HR_data.R - Calculates distance to nearest non-self study site with resistant plants ("near_infect") and merges derived management variables to glyphosate resistance rates and field attributes for each site. Exports output to "CompiledData/HR_merged_by_site.csv".

D_CART.R - Runs classification and regression tree (CART) analyses. Plots fitted trees and tables to "Figures" and "Tables" directories which it creates if they don't already exixt. 

E_PastMgt_spatial.R - Runs semipartial correlation analyses and fits generalized linear mixed models to management data. Conducts AICc-based model ranking and plots fitted models. Outputs relevant tables and figures as above. 

F_herbicide_diversity_plots.R - Plots and analyzes herbicide turnover and beta diversity metrics. Outputs tables and figures as above.


**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//
To run the scripts and be certain they can find required data and output results as expected, be sure to set the working directory to the Evans_et_al_Code_Supplement folder containing this README and the R scripts. The easiest way to do this is to open R by clicking on one of the files in the Supplement directory.
**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//

The general structure of each script is the same:
1: clear workspace, set working directory, load required packages and scripts.
2: set options such as T/F switches to save output, number of sites/cluster, etc...
3: load data
4: process/organize/subset data
5: run analyses
6: create tables and figures
7: save output

**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//**\\**//
The scripts require a number of additional packages which are loaded in the beginning section of each file. Packrat should manage these for you. Below is the output of the sessionInfo() command showing all the packages and their versions required to run the entire suite of analyses:
 
> sessionInfo()
R version 3.0.2 (2013-09-25)
Platform: x86_64-w64-mingw32/x64 (64-bit)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] parallel  grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] car_2.0-19        pwr_1.1.1         ppcor_1.0         plyr_1.8          psych_1.3.12      gridExtra_0.9.1  
 [7] lme4_1.0-5        Matrix_1.1-1.1    xlsx_0.5.5        xlsxjars_0.5.0    rJava_0.9-6       e1071_1.6-1      
[13] class_7.3-11      caret_6.0-35      ggplot2_0.9.3.1   lattice_0.20-29   doSNOW_1.0.9      snow_0.3-13      
[19] iterators_1.0.6   foreach_1.4.1     rpart.plot_1.4-3  rpart_4.1-8       fields_6.9.1      maps_2.3-6       
[25] spam_0.40-0       AICcmodavg_1.35   data.table_1.8.10

loaded via a namespace (and not attached):
 [1] BradleyTerry2_1.0-5 brglm_0.5-9         codetools_0.2-8     colorspace_1.2-4    compiler_3.0.2     
 [6] dichromat_2.0-0     digest_0.6.4        gtable_0.1.2        gtools_3.1.1        labeling_0.2       
[11] MASS_7.3-33         minqa_1.2.2         munsell_0.4.2       nlme_3.1-117        nnet_7.3-8         
[16] packrat_0.4.1-1     proto_0.3-10        RColorBrewer_1.0-5  reshape2_1.2.2      scales_0.2.3       
[21] splines_3.0.2       stringr_0.6.2       tools_3.0.2        
> 
Packages can be installed by typing install.packages("package name"). Note that some analyses are sensitive the versions of certain packages used. In particular, the syntax of lme4 changed after version 1.0, and some details of caret changed with version 6.0-22, which I used. To install or update all packages used to the current versions run this command in R:

install.packages("data.table","rpart","rpart.plot","parallel","doSNOW","caret","ggplot2","lme4","gridExtra","AICcmodavg","psych","plyr","fields","ppcor", "pwr","car","rdgal","maps","mapproj","rgeos","grid")

You may have to uninstall Note that if you're running this 20 years from now you'll want to dig up the versions used originally, as they are likely to have changed substantially!

In addition, several analyses require source code proved in the "Supporting_Code" directory.

If you have these packages installed and have set the working directory so it points to the "Evans_et_al_Code_Supplement" directory containing this file, the supporting code, and all the R scripts, the analyses should work and produce the figures and tables exactly as presented in the paper.

Good luck!
Jeff Evans
October 13, 2014
