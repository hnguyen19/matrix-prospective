---
output:
  bookdown::word_document2:
#  bookdown::html_document2:
      toc: false
      fig_caption: yes
      keep_md: true
bibliography: WH-pop-dynamics.bib
csl: apa-no-ampersand.csl 
---

The data in the model projection was used in this simulation. 100 iterations of simulation were run per each rotation crossed with corn weed management regime.  

Goal: manipulate mature plant density for cohorts 1 through 3 via survival rate manipulation



























```r
# event sequence: seed dropped - field cultivator - emerge - survive - new seed - chisel - overwinter 

# create a function 
# vec: starting seed column
# poh: post-harvest tillage
# ow: over winter seed survival
# prt: pre-planting-tillage
# em: emergence
# sv: seed survival rate and seedling to maturity success rate
# seed: fecundity

rot_2year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S){
  

  sv_C[3,3] <- .006 #99.4 % efficacy wrt plant density
  sv_C[4,4] <- .006
  sv_C[5,5] <- .006
 # sv_C[6,6] <- .01
#  sv_C[7,7] <- .01



  sv_S[3,3] <- .006
  sv_S[4,4] <- .006
  sv_S[5,5] <- .006
#  sv_S[6,6] <- .01
#  sv_S[7,7] <- .01


 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 

   
# soybean phase dynamics

   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 

   after_soy
}

rot_2year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S){
  


  sv_C[3,3] <- .001 #99.9 % efficacy
  sv_C[4,4] <- .002
  sv_C[5,5] <- .003 # 99.8% efficacy
 # sv_C[6,6] <- .01
#  sv_C[7,7] <- .01



  sv_S[3,3] <- .001
  sv_S[4,4] <-  .002
  sv_S[5,5] <-  .003
#  sv_S[6,6] <- .01
#  sv_S[7,7] <- .01

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
# soybean phase dynamics

   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 

   after_soy
}
```


```r
##### with corn under conventional weed management {-}
t <- 100
N_2yr_conv <- list() # blank data frame to save loop output 
N_2yr_conv[[1]] <- starting_point 

for (i in 2:t) { 
  N_2yr_conv[[i]] = rot_2year_conv(vec = N_2yr_conv[[i-1]],
                              poh_C = fall_tillage$C2_conv,
                              ow_C = overwinter$C2_conv,
                              prt_C  = spring_tillage$C2_conv,
                              em_C  = emergence$C2_conv,
                              sv_C = summer_survival$C2_conv,
                              seed_C = fecundity18$C2_conv,
                              
                              #soybean dynamics   
                              poh_S = fall_tillage$S2_conv,
                              ow_S = overwinter$S2_conv,
                              prt_S  = spring_tillage$S2_conv,
                              em_S  = emergence$S2_conv,
                              sv_S = summer_survival$S2_conv,
                              seed_S = fecundity18$S2_conv)
}

N_2yr_conv_df <-  N_2yr_conv %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
    dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_6"),t)) %>%
    filter(category %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "2-year",
         Corn_weed_management = "conventional") %>%
    na.omit() 
```
 

 

```r
##### with corn under low herbicide weed management {-}
N_2yr_low <- list() # blank dataframe to save loop output 

N_2yr_low[[1]] <- starting_point 
for (i in 2:t) { 
  N_2yr_low[[i]] = rot_2year_low(vec = N_2yr_low[[i-1]],
                             poh_C = fall_tillage$C2_low,
                             ow_C = overwinter$C2_low,
                             prt_C  = spring_tillage$C2_low,
                             em_C  = emergence$C2_low,
                             sv_C = summer_survival$C2_low,
                             seed_C = fecundity18$C2_low,
                             
                             #soybean dynamics   
                             poh_S = fall_tillage$S2_low,
                             ow_S = overwinter$S2_low,
                             prt_S  = spring_tillage$S2_low,
                             em_S  = emergence$S2_low,
                             sv_S = summer_survival$S2_low,
                             seed_S = fecundity18$S2_low)
}

N_2yr_low_df <-  N_2yr_low %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
    dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_6"),t)) %>%
    filter(category %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "2-year",
         Corn_weed_management = "low") %>%
    na.omit() 
```
  



```r
rot_3year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O){
  


  sv_C[3,3] <- .0001
  sv_C[4,4] <- .0001
  sv_C[5,5] <- .0001
  sv_C[6,6] <- .0001
  sv_C[7,7] <- .0001
  sv_C[8,8] <- .0001



  sv_S[3,3] <- .0001
  sv_S[4,4] <- .0001
  sv_S[5,5] <- .0001
  sv_S[6,6] <- .0001
  sv_S[7,7] <- .0001
  sv_S[8,8] <- .0001
  
## Extra control efficacy in oat is now needed  
  sv_O[5,5] <- .01
  sv_O[6,6] <- .01
  sv_O[7,7] <- .01
  sv_O[8,8] <- .01

# corn phase dynamics  
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*%  vec 
 # soybean phase dynamics
   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%   after_corn
# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  after_oat
}

### low herbicide weed management
## Manipulation note: if cohorts 1 through 3 were reduced to rlnorm(1, 2.65, 0.89), alphas are around 0.5 --> super "safe", but hard
## cohorts 1 through 3 at rlnorm(1, 5.2, 0.51): more realistic
rot_3year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O){
  


  sv_C[3,3] <- .0001
  sv_C[4,4] <- .003
  sv_C[5,5] <- .003
#  sv_C[6,6] <- .0001
#  sv_C[7,7] <- .0001
#  sv_C[8,8] <- .0001



  sv_S[3,3] <- .0001
  sv_S[4,4] <- .003
  sv_S[5,5] <- .003
 # sv_S[6,6] <- .0001
 # sv_S[7,7] <- .0001
#  sv_S[8,8] <- .001

  
# corn phase dynamics  
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*%  vec 
 # soybean phase dynamics
   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%   after_corn
# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  after_oat
}
```


```r
##### with corn under conventional weed management {-}
N_3yr_conv <- list() # blank dataframe to save loop output 

N_3yr_conv[[1]] <- starting_point 


for (i in 2:t) { 
  N_3yr_conv[[i]] = rot_3year_conv(vec = N_3yr_conv[[i-1]],
                              poh_C = fall_tillage$C3_conv,
                              ow_C = overwinter$C3_conv,
                              prt_C  = spring_tillage$C3_conv,
                              em_C  = emergence$C3_conv,
                              sv_C = summer_survival$C3_conv,
                              seed_C = fecundity18$C3_conv,
                              
                              #soybean dynamics   
                              poh_S = fall_tillage$S3_conv,
                              ow_S = overwinter$S3_conv,
                              prt_S  = spring_tillage$S3_conv,
                              em_S  = emergence$S3_conv,
                              sv_S = summer_survival$S3_conv,
                              seed_S = fecundity18$S3_conv,
                              
                              #oat dynamics   
                              poh_O = fall_tillage$O3_conv,
                              ow_O = overwinter$O3_conv,
                              prt_O  = spring_tillage$O3_conv,
                              em_O  = emergence$O3_conv,
                              sv_O = summer_survival$O3_conv,
                              seed_O = fecundity18$O3_conv)
}

N_3yr_conv_df <-  N_3yr_conv %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
    dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_6"),t)) %>%
    filter(category %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = nthroot(lambda_cycle,3),
          Rotation = "3-year",
         Corn_weed_management = "conventional") %>%
    na.omit() 
```
 


```r
##### with corn under low herbicide weed management {-} 
N_3yr_low <- list() # blank dataframe to save loop output 

N_3yr_low[[1]] <- starting_point 


for (i in 2:t) { 
  N_3yr_low[[i]] = rot_3year_low(vec = N_3yr_low[[i-1]],
                             poh_C = fall_tillage$C3_conv,
                             ow_C = overwinter$C3_low,
                             prt_C  = spring_tillage$C3_low,
                             em_C  = emergence$C3_low,
                             sv_C = summer_survival$C3_low,
                             seed_C = fecundity18$C3_low,
                             
                             #soybean dynamics   
                             poh_S = fall_tillage$S3_low,
                             ow_S = overwinter$S3_low,
                             prt_S  = spring_tillage$S3_low,
                             em_S  = emergence$S3_low,
                             sv_S = summer_survival$S3_low,
                             seed_S = fecundity18$S3_low,
                             
                             #oat dynamics   
                             poh_O = fall_tillage$O3_low,
                             ow_O = overwinter$O3_low,
                             prt_O  = spring_tillage$O3_low,
                             em_O  = emergence$O3_low,
                             sv_O = summer_survival$O3_low,
                             seed_O = fecundity18$O3_low)
}

N_3yr_low_df <-  N_3yr_low %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
    dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_6"),t)) %>%
    filter(category %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = nthroot(lambda_cycle,3),
          Rotation = "3-year",
         Corn_weed_management = "low") %>%
    na.omit()
```





```r
### conventional weed management
rot_4year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){


  sv_C[3,3] <- .0001
  sv_C[4,4] <- .001
  sv_C[5,5] <- .001
  sv_C[6,6] <- .005
  sv_C[7,7] <- .005
  sv_C[8,8] <- .005



  sv_S[3,3] <- .0001
  sv_S[4,4] <- .001
  sv_S[5,5] <- .001
  sv_S[6,6] <- .005
  sv_S[7,7] <- .005
  sv_S[8,8] <- .005

# corn phase dynamics  
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*%  vec 
 # soybean phase dynamics
   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%   after_corn
# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
# alfalfa phase dynamics
after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  after_alfalfa
}

### low herbicide weed management
rot_4year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  


  sv_C[3,3] <- .0001
  sv_C[4,4] <- .001
  sv_C[5,5] <- .009
  sv_C[6,6] <- .05




  sv_S[3,3] <- .0001
  sv_S[4,4] <- .001
  sv_S[5,5] <- .009
  sv_S[6,6] <- .05


# corn phase dynamics  
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*%  vec 
 # soybean phase dynamics
   after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%   after_corn
# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
# alfalfa phase dynamics
after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
   
  after_alfalfa
}
```


```r
##### with corn under conventional weed management {-}
N_4yr_conv <- list() # blank dataframe to save loop output 

N_4yr_conv[[1]] <- starting_point 

for (i in 2:t) { 
  N_4yr_conv[[i]] = rot_4year_conv(vec = N_4yr_conv[[i-1]],
                              poh_C = fall_tillage$C4_conv,
                              ow_C = overwinter$C4_conv,
                              prt_C  = spring_tillage$C4_conv,
                              em_C  = emergence$C4_conv,
                              sv_C = summer_survival$C4_conv,
                              seed_C = fecundity18$C4_conv,
                              
                              #soybean dynamics   
                              poh_S = fall_tillage$S4_conv,
                              ow_S = overwinter$S4_conv,
                              prt_S  = spring_tillage$S4_conv,
                              em_S  = emergence$S4_conv,
                              sv_S = summer_survival$S4_conv,
                              seed_S = fecundity18$S4_conv,
                              
                              #oat dynamics   
                              poh_O = fall_tillage$O4_conv,
                              ow_O = overwinter$O4_conv,
                              prt_O  = spring_tillage$O4_conv,
                              em_O  = emergence$O4_conv,
                              sv_O = summer_survival$O4_conv,
                              seed_O = fecundity18$O4_conv,
                              
                              #alfalfa dynamics   
                          poh_A = fall_tillage$A4_conv,
                          ow_A = overwinter$A4_conv,
                          prt_A  = spring_tillage$A4_conv,
                          em_A  = emergence$A4_conv,
                          sv_A = summer_survival$A4_conv,
                          seed_A = fecundity18$A4_conv)
}

N_4yr_conv_df <-  N_4yr_conv %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
  dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_4", "cohort_3", "cohort_5", "cohort_6"),t)) %>%
  filter(category %in% c("top", "bottom")) %>%
  unnest(cols = everything() ) %>%
  mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = nthroot(lambda_cycle,4),
         Rotation = "4-year",
         Corn_weed_management = "conventional") %>%
  na.omit() 
```
 


```r
##### with corn under low herbicide weed management {-} 
N_4yr_low <- list() # blank dataframe to save loop output 

N_4yr_low[[1]] <- starting_point 

for (i in 2:t) { 
  N_4yr_low[[i]] = rot_4year_low(vec = N_4yr_low[[i-1]],
                              poh_C = fall_tillage$C4_low,
                              ow_C = overwinter$C4_low,
                              prt_C  = spring_tillage$C4_low,
                              em_C  = emergence$C4_low,
                              sv_C = summer_survival$C4_low,
                              seed_C = fecundity18$C4_low,
                              
                              #soybean dynamics   
                              poh_S = fall_tillage$S4_low,
                              ow_S = overwinter$S4_low,
                              prt_S  = spring_tillage$S4_low,
                              em_S  = emergence$S4_low,
                              sv_S = summer_survival$S4_low,
                              seed_S = fecundity18$S4_low,
                              
                              #oat dynamics   
                              poh_O = fall_tillage$O4_low,
                              ow_O = overwinter$O4_low,
                              prt_O  = spring_tillage$O4_low,
                              em_O  = emergence$O4_low,
                              sv_O = summer_survival$O4_low,
                              seed_O = fecundity18$O4_low,
                              
                              #alfalfa dynamics   
                              poh_A = fall_tillage$A4_low,
                              ow_A = overwinter$A4_low,
                              prt_A  = spring_tillage$A4_low,
                              em_A  = emergence$A4_low,
                              sv_A = summer_survival$A4_low,
                              seed_A = fecundity18$A4_low)
}

N_4yr_low_df <-  N_4yr_low %>% 
  unlist(recursive = FALSE) %>%
  data.frame() %>%
  dplyr::rename(counts = ".") %>%
  dplyr::mutate(category = rep(c("top", "bottom", "cohort_1", "cohort_2", "cohort_3", "cohort_4", "cohort_5", "cohort_6"),t)) %>%
  filter(category %in% c("top", "bottom")) %>%
  unnest(cols = everything() ) %>%
  mutate(cycle_no = rep(1:t, each = 2)) %>%
  group_by(category) %>%
  mutate(lambda_cycle = counts/lag(counts),
         lambda_annualized = nthroot(lambda_cycle,4),
         Rotation = "4-year",
         Corn_weed_management = "low") %>%
  na.omit() 
```


![Figure 1: Population growth rates over 100 rotational cycles. All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. The simulation applied weed management on cohorts 1 through 3 in corn and soybean only. It was expected that only the survival rate of cohorts 1 through 3 fecundity in corn and soybean were manipulated to find the mature plant thresholds, and that the survival rates of cohorts 4 and beyond in corn and soybean were kept as they were measured from 2018. However, additional control was neccessary in some crop phases. The crop phases marked with an asterisk (*) are where control measures extended beyond waterhemp cohort 3 would be neccessary. The crop phases marked with an inverted comma (') are where no additional control was applied.  Each panel was annotated with the average mature plant density (plants/m2) for the first three plant cohorts and the whole crop phase. The red horizontal line marks lambda = 1.](Q1-mature-density-allowance-rot_files/figure-docx/mature-density-sim-lambda-plot-1.png)



```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![Figure 2: Population size at the end of a rotation cycle over 100 rotational cycles (the 2-year rotation ended at the soybean phase, the 3-year rotation ended at the oat phase, and the 4-year rotation ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. The simulation applied improved weed control efficacy on cohorts 1 through 3 in corn and soybean only. The relationships of aboveground mass and fecundity in Nguyen and Liebman (2022a) were used to estimate cohort-based fecundity. It was expected that no waterhemp cohorts in any crop environments but only the cohorts 1 through 3 in corn and soybean had their survival rates manipulated to find the mature plant density thresholds. However, additional control efficacy was needed in some crop phases outside of the expected groups to reduced the mature plant densities. The crop phases marked with an asterik (*) are where control measures extended beyond the expected cohorts within the expected crop environments would be neccessary. Each panel was annotated with the average fecundity thresholds for the first three waterhemp cohorts in corn and soybean followed by the whole crop phase. The crop phases marked with an inverted comma (') are where no additional control was applied. The red horizontal line marks lambda = 1.](Q1-mature-density-allowance-rot_files/figure-docx/mature-density-sim-N-plot-1.png)



