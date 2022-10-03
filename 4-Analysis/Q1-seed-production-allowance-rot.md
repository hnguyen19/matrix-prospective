---
output:
#  bookdown::word_document2:
  bookdown::html_document2:
      toc: false
      fig_caption: yes
      keep_md: true
bibliography: WH-pop-dynamics.bib
csl: apa-no-ampersand.csl 
---

The data in the model projection was used in this simulation. 100 iterations of simulation were run per each rotation crossed with corn weed management regime. Different simulation outputs are collected at different chunks: x-lambda-x-x for population growth rate, x-seed-production-per-capita for manipulated seed production per plant, x-plant-density-fixed for mature plant density as empirically measure, and x-all-output for all the outputs combined. 


















```r
# event sequence: seed dropped - field cultivator - emerge - survive - new seed - chisel - overwinter 

# create a function 
# vec: starting seed column
# prt: pre-planting-tillage
# em: emergence
# sv: seed survival rate and seedling to maturity success rate
# seed: fecundity
# poh: post-harvest tillage
# ow: over winter seed survival
##### with corn under conventional weed management {-}

### Output: seedbank at top and bottom strata at the end of each crop phase

N_2yr_conv_lambda <- list() # blank data frame to save loop output 
N_2yr_conv_lambda[[1]] <- starting_point 

for (i in 2:t) { 
  N_2yr_conv_lambda[[i]] = rot_2year_conv_lambda(vec = N_2yr_conv_lambda[[i-1]],
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

N_2yr_conv_lambda_df <-  N_2yr_conv_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom", 
                                "cohort_1", "cohort_2", 
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything())  %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "2-year",
         Corn_weed_management = "conventional") 

##### with corn under low herbicide weed management {-}
N_2yr_low_lambda <- list() # blank data frame to save loop output 

N_2yr_low_lambda[[1]] <- starting_point 
for (i in 2:t) { 
  N_2yr_low_lambda[[i]] = rot_2year_low_lambda(vec = N_2yr_low_lambda[[i-1]],
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

N_2yr_low_lambda_df <-  N_2yr_low_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom", 
                                "cohort_1", "cohort_2", 
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything())  %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "2-year",
         Corn_weed_management = "low") 
```



```r
### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix
N_2yr_conv_mature_plant_density_fixed <- rot_2year_conv_plant_density_fixed(vec = starting_point ,
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

N_2yr_conv_mature_plant_density_fixed_df <- N_2yr_conv_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"),2)) %>%
   mutate(Crop_ID = c(rep("C2", 6), rep("S2", 6)))


### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_2yr_low_mature_plant_density_fixed <- rot_2year_low_plant_density_fixed(vec = starting_point ,
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

N_2yr_low_mature_plant_density_fixed_df <- N_2yr_low_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"),2)) %>%
   mutate(Crop_ID = c(rep("C2", 6), rep("S2", 6)))
```


```r
### CONV Output: seed production, after manipulation by cohort x crop phase x iteration

N_2yr_conv_seed_production_per_capita <- list() # blank data frame to save loop output 

for (i in 1:t) { 
  N_2yr_conv_seed_production_per_capita[[i]] =
    rot_2year_conv_seed_production_per_cap(seed_C = fecundity18$C2_conv,
                                           seed_S = fecundity18$S2_conv)
}

# original per capita seed production in fecundity18$C2_conv[1,3:8] and fecundity18$S2_conv[1,3:8]
N_2yr_conv_seed_production_per_capita_df <- N_2yr_conv_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*2)) %>%
   mutate(cycle_no = as.character(rep(1:t, each = 12))) %>% # 12 = 6 cohorts x 2 phases
   mutate(Crop_ID = rep(c(rep("C2", 6), rep("S2", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C2_conv[1,3:8],
                                          fecundity18$S2_conv[1,3:8]), t)) %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))
  

### LOW output

N_2yr_low_seed_production_per_capita <- list() # blank data frame to save loop output 

for (i in 1:t) { 
  N_2yr_low_seed_production_per_capita[[i]] =
    rot_2year_low_seed_production_per_cap(seed_C = fecundity18$C2_low,
                                          seed_S = fecundity18$S2_low)
}


N_2yr_low_seed_production_per_capita_df <- N_2yr_low_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*2)) %>%
   mutate(cycle_no = as.character(rep(1:t, each = 12))) %>% # 12 = 6 cohorts x 2 phases
   mutate(Crop_ID = rep(c(rep("C2", 6), rep("S2", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C2_low[1,3:8],
                                          fecundity18$S2_low[1,3:8]), t)) %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))
```




```r
# total stratified seedbank density and lambdas in N_2yr_conv_lambda_df
# manipulated seed production in N_2yr_conv_seed_production_per_capita_df
# fixed mature plant density in N_2yr_conv_mature_plant_density_fixed_df 

N_2yr_conv_all_df <- N_2yr_conv_seed_production_per_capita_df %>% 
  left_join(N_2yr_conv_lambda_df, by = "cycle_no") %>%
  left_join(N_2yr_conv_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort")) 


N_2yr_low_all_df <- N_2yr_low_seed_production_per_capita_df %>% 
  left_join(N_2yr_low_lambda_df, by = "cycle_no") %>%
  left_join(N_2yr_low_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort")) 
```




```r
##### with corn under conventional weed management {-}
N_3yr_conv_lambda <- list() # blank dataframe to save loop output 

N_3yr_conv_lambda[[1]] <- starting_point 


for (i in 2:t) { 
  N_3yr_conv_lambda[[i]] = rot_3year_conv_lambda(vec = N_3yr_conv_lambda[[i-1]],
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

N_3yr_conv_lambda_df <-  N_3yr_conv_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom",
                                "cohort_1", "cohort_2",
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "3-year",
         Corn_weed_management = "conventional") 

##### with corn under low herbicide weed management {-} 
N_3yr_low_lambda <- list() # blank dataframe to save loop output 

N_3yr_low_lambda[[1]] <- starting_point 


for (i in 2:t) { 
  N_3yr_low_lambda[[i]] = rot_3year_low_lambda(vec = N_3yr_low_lambda[[i-1]],
                             poh_C = fall_tillage$C3_low,
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

N_3yr_low_lambda_df <-  N_3yr_low_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom",
                                "cohort_1", "cohort_2",
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything() ) %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "3-year",
         Corn_weed_management = "low") 
```



```r
### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix
N_3yr_conv_mature_plant_density_fixed <- rot_3year_conv_plant_density_fixed(vec = starting_point ,
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

N_3yr_conv_mature_plant_density_fixed_df <- N_3yr_conv_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 3)) %>% # 3 phases
   mutate(Crop_ID = c(rep("C3", 6), rep("S3", 6), rep("O3", 6)))


### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix
N_3yr_low_mature_plant_density_fixed <- rot_3year_low_plant_density_fixed(vec = starting_point,
                             poh_C = fall_tillage$C3_low,
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

N_3yr_low_mature_plant_density_fixed_df <- N_3yr_low_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 3)) %>% #3 phases
   mutate(Crop_ID = c(rep("C3", 6), rep("S3", 6), rep("O3", 6)))
```




```r
### CONV Output: seed production, after manipulation by cohort x crop phase x iteration
N_3yr_conv_seed_production_per_capita <- list() # blank data frame to save loop output 


for (i in 1:t) { 
  N_3yr_conv_seed_production_per_capita[[i]] =
    rot_3year_conv_seed_production_per_cap(seed_C = fecundity18$C3_conv,
                                           seed_S = fecundity18$S3_conv,
                                           seed_O = fecundity18$O3_conv)
}

# original per capita seed production in fecundity18$C3_conv[1,3:8], fecundity18$S3_conv[1,3:8], and fecundity18$O3_conv
N_3yr_conv_seed_production_per_capita_df <- N_3yr_conv_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*3)) %>% #*3 for 3 phases
   mutate(cycle_no = as.character(rep(1:t, each = 18))) %>% # 18 = 6 cohorts x 3 phases
   mutate(Crop_ID = rep(c(rep("C3", 6), rep("S3", 6), rep("O3", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C3_conv[1,3:8],
                                          fecundity18$S3_conv[1,3:8],
                                          fecundity18$O3_conv[1,3:8]), t)) %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))

### LOW output

N_3yr_low_seed_production_per_capita <- list() # blank data frame to save loop output 


for (i in 1:t) { 
  N_3yr_low_seed_production_per_capita[[i]] =
    rot_3year_low_seed_production_per_cap(seed_C = fecundity18$C3_low,
                                          seed_S = fecundity18$S3_low,
                                          seed_O = fecundity18$O3_low)
}


N_3yr_low_seed_production_per_capita_df <- N_3yr_low_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*3)) %>% #*3 for 3 phases
   mutate(cycle_no = as.character(rep(1:t, each = 18))) %>% # 18 = 6 cohorts x 3 phases
   mutate(Crop_ID = rep(c(rep("C3", 6), rep("S3", 6), rep("O3", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C3_low[1,3:8],
                                          fecundity18$S3_low[1,3:8],
                                          fecundity18$O3_low[1,3:8]), t)) %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))
```



```r
# total stratified seedbank density and lambdas in N_3yr_conv_df
# manipulated seed production in N_3yr_conv_seed_production_per_capita_df
# fixed mature plant density in N_3yr_conv_mature_plant_density_fixed_df 

N_3yr_conv_all_df <- N_3yr_conv_seed_production_per_capita_df %>% 
  left_join(N_3yr_conv_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort")) %>%
  left_join(N_3yr_conv_lambda_df, by = "cycle_no")


N_3yr_low_all_df <- N_3yr_low_seed_production_per_capita_df %>% 
  left_join(N_3yr_low_lambda_df, by = "cycle_no") %>%
  left_join(N_3yr_low_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort"))  
```




```r
##### with corn under conventional weed management {-}
N_4yr_conv_lambda <- list() # blank dataframe to save loop output 

N_4yr_conv_lambda[[1]] <- starting_point 

for (i in 2:t) { 
  N_4yr_conv_lambda[[i]] = rot_4year_conv_lambda(vec = N_4yr_conv_lambda[[i-1]],
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

N_4yr_conv_lambda_df <-  N_4yr_conv_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom",
                                "cohort_1", "cohort_2",
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything())  %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = sqrt(lambda_cycle),
          Rotation = "4-year",
         Corn_weed_management = "conventional") 

##### with corn under low herbicide weed management {-} 
N_4yr_low_lambda <- list() # blank dataframe to save loop output 

N_4yr_low_lambda[[1]] <- starting_point 

for (i in 2:t) { 
  N_4yr_low_lambda[[i]] = rot_4year_low_lambda(vec = N_4yr_low_lambda[[i-1]],
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

N_4yr_low_lambda_df <-  N_4yr_low_lambda %>% 
  unlist(recursive = TRUE) %>%
  data.frame() %>%
    dplyr::rename(seedbank_counts = ".") %>%
  dplyr::mutate(stratum = rep(c("top", "bottom",
                                "cohort_1", "cohort_2",
                                "cohort_3", "cohort_4",
                                "cohort_5", "cohort_6"),t)) %>%
    filter(stratum %in% c("top", "bottom")) %>%
    unnest(cols = everything())  %>%
    mutate(cycle_no = as.character(rep(1:t, each = 2))) %>%
  pivot_wider(names_from = stratum, values_from = seedbank_counts) %>%
  mutate(total_seedbank_counts = top + bottom) %>%
  mutate(lambda_cycle = total_seedbank_counts/lag(total_seedbank_counts),
         lambda_annualized = nthroot(lambda_cycle, 4),
         Rotation = "4-year",
         Corn_weed_management = "low")
```



```r
### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix
N_4yr_conv_mature_plant_density_fixed <- rot_4year_conv_plant_density_fixed(vec = starting_point ,
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

N_4yr_conv_mature_plant_density_fixed_df <- N_4yr_conv_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 4)) %>% #4phases
   mutate(Crop_ID = c(rep("C4", 6), rep("S4", 6), rep("O4", 6), rep("A4", 6)))


### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix


N_4yr_low_mature_plant_density_fixed <- rot_4year_low_plant_density_fixed(vec = starting_point,
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

N_4yr_low_mature_plant_density_fixed_df <- N_4yr_low_mature_plant_density_fixed  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(plant_counts_fixed = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 4)) %>% # 4 phases
   mutate(Crop_ID = c(rep("C4", 6), rep("S4", 6), rep("O4", 6), rep("A4", 6)))
```


```r
### CONV Output: seed production, after manipulation by cohort x crop phase x iteration


N_4yr_conv_seed_production_per_capita <- list() # blank data frame to save loop output 


for (i in 1:t) { 
  N_4yr_conv_seed_production_per_capita[[i]] =
    rot_4year_conv_seed_production_per_cap(seed_C = fecundity18$C4_conv,
                                           seed_S = fecundity18$S4_conv,
                                           seed_O = fecundity18$O4_conv,
                                           seed_A = fecundity18$A4_conv)
}

# original per capita seed production in fecundity18$X4_conv[1,3:8], X = C, S, O, or A
N_4yr_conv_seed_production_per_capita_df <- N_4yr_conv_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*4)) %>% #*4 for 4 phases
   mutate(cycle_no = as.character(rep(1:t, each = 24))) %>% # 24 = 6 cohorts x 4 phases
   mutate(Crop_ID = rep(c(rep("C4", 6), rep("S4", 6), rep("O4", 6), rep("A4", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C4_conv[1,3:8],
                                          fecundity18$S4_conv[1,3:8],
                                          fecundity18$O4_conv[1,3:8],
                                          fecundity18$A4_conv[1,3:8]), t))  %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))

### LOW output


N_4yr_low_seed_production_per_capita <- list() # blank data frame to save loop output 


for (i in 1:t) { 
  N_4yr_low_seed_production_per_capita[[i]] =
    rot_4year_low_seed_production_per_cap(seed_C = fecundity18$C4_low,
                                          seed_S = fecundity18$S4_low,
                                          seed_O = fecundity18$O4_low,
                                          seed_A = fecundity18$A4_low)
}


N_4yr_low_seed_production_per_capita_df <- N_4yr_low_seed_production_per_capita  %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>% #corn was listed before soybean in the customized f.
  dplyr::rename(seed_per_capita_threshold = ".") %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), t*4)) %>% #*4 for 4 phases
   mutate(cycle_no = as.character(rep(1:t, each = 24))) %>%
   mutate(Crop_ID = rep(c(rep("C4", 6), rep("S4", 6), rep("O4", 6), rep("A4", 6)), t)) %>%
  mutate(seed_per_capita_original = rep(c(fecundity18$C4_low[1,3:8],
                                          fecundity18$S4_low[1,3:8],
                                          fecundity18$O4_low[1,3:8],
                                          fecundity18$A4_low[1,3:8]), t))  %>%
  mutate(seed_production_manipulated = ifelse(seed_per_capita_threshold == seed_per_capita_original, "no", "yes"))
```



```r
# total stratified seedbank density and lambdas in N_4yr_conv_lambda_df
# manipulated seed production in N_4yr_conv_seed_production_per_capita_df
# fixed mature plant density in N_4yr_conv_mature_plant_density_fixed_df 

N_4yr_conv_all_df <- N_4yr_conv_seed_production_per_capita_df %>% 
  left_join(N_4yr_conv_lambda_df, by = "cycle_no") %>%
  left_join(N_4yr_conv_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort"))  


N_4yr_low_all_df <- N_4yr_low_seed_production_per_capita_df %>% 
  left_join(N_4yr_low_lambda_df, by = "cycle_no") %>%
  left_join(N_4yr_low_mature_plant_density_fixed_df, by = c("Crop_ID", "cohort")) 
```



```
## `summarise()` has grouped output by 'Crop_ID', 'Corn_weed_management'. You can
## override using the `.groups` argument.
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
```

```{=html}
<template id="39f36dd9-201c-4265-9ae1-276fde8cb4dc"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}
</style><div class="tabwid"><style>.cl-0fddce9a{}.cl-0fcd04e8{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-0fd651b0{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-0fd651ba{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-0fd66e7a{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fd66e84{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fd66e85{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fd66e8e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fd66e8f{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fd66e98{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fb75936{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-0fddce9a'>

```

<caption>(\#tab:seed-allowance-combined)<span class="cl-0fb75936">Added control efficacy (with respect to the original control efficacy reflected by the unmanipulated seed production) averaged over 10000 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seeds in the bottom 2 - 20 cm soil stratum.</span></caption>

```{=html}

<thead><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e7a"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">Crop ID</span></p></td><td class="cl-0fd66e7a"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">Corn weed management</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 1</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 2</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 2</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 4</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 5</span></p></td><td class="cl-0fd66e84"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">cohort 6</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C2</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C2</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.98</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S2</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S2</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.93</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.96</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.59</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">O3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">O3</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">C4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.99</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.97</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.77</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">S4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">1</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">O4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">O4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">A4</span></p></td><td class="cl-0fd66e85"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">conventional</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e8e"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-0fd66e8f"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">A4</span></p></td><td class="cl-0fd66e8f"><p class="cl-0fd651b0"><span class="cl-0fcd04e8">low</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0.00</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td><td class="cl-0fd66e98"><p class="cl-0fd651ba"><span class="cl-0fcd04e8">0</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="be63834f-8fa8-4399-ad10-478a59e60695"></div>
<script>
var dest = document.getElementById("be63834f-8fa8-4399-ad10-478a59e60695");
var template = document.getElementById("39f36dd9-201c-4265-9ae1-276fde8cb4dc");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```


```r
seed_allowance_sim_df %>% 
  group_by(Crop_ID, Corn_weed_management, cohort) %>%
  summarise(mean_seed_per_capita_threshold = mean(seed_per_capita_threshold)) %>%
  pivot_wider(names_from = cohort, values_from = mean_seed_per_capita_threshold) %>%
  mutate(phase_order = ifelse(str_detect(Crop_ID, "C"), 1,
                              ifelse(str_detect(Crop_ID, "S"), 2,
                                     ifelse(str_detect(Crop_ID, "O"), 3, 4))))  %>%
  mutate(Rot = substr(Crop_ID,2,2))%>%
  arrange(Rot, phase_order, Corn_weed_management)
```

```
## `summarise()` has grouped output by 'Crop_ID', 'Corn_weed_management'. You can
## override using the `.groups` argument.
```

```
## # A tibble: 18 × 10
## # Groups:   Crop_ID, Corn_weed_management [18]
##    Crop_ID Corn_weed_…¹    `1`    `2`    `3`    `4`     `5`    `6` phase…² Rot  
##    <chr>   <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <chr>
##  1 C2      conventional  111.  1.10e2 8.40e1 6.30e3 2.19e+3 2.72e2       1 2    
##  2 C2      low            21.2 2.10e1 2.12e1 5.06e2 2.24e+2 3.48e1       1 2    
##  3 S2      conventional  111.  1.11e2 8.49e1 1.90e4 6.50e+3 1.76e3       2 2    
##  4 S2      low            21.5 2.14e1 2.13e1 2.17e4 5.37e+3 1.40e3       2 2    
##  5 C3      conventional   21.6 2.16e1 2.13e1 1.66e2 2.71e+2 7.52e1       1 3    
##  6 C3      low            21.2 2.14e1 8.41e1 1.44e2 3.55e+1 5.12e1       1 3    
##  7 S3      conventional   20.8 2.14e1 2.13e1 2.13e1 2.13e+1 2.10e1       2 3    
##  8 S3      low            21.7 2.10e1 1.13e3 3.14e4 2.80e+4 5.62e3       2 3    
##  9 O3      conventional 3354.  8.49e2 4.25e2 2.18e2 1.23e+2 7.96e1       3 3    
## 10 O3      low           658   2.37e2 3.08e2 2.06e2 1.61e+2 1.05e2       3 3    
## 11 C4      conventional   20.8 2.12e1 2.15e1 7.22e2 5.01e+2 2.73e2       1 4    
## 12 C4      low            21.5 2.12e1 2.09e1 1.65e2 5.22e+1 1.88e1       1 4    
## 13 S4      conventional   21.3 2.13e1 2.12e1 1.70e3 5.40e+3 1.89e4       2 4    
## 14 S4      low            20.9 2.15e1 2.11e1 2.13e1 2.14e+1 4.67e3       2 4    
## 15 O4      conventional 3697.  1.02e3 2.67e2 2.27e2 3.63e+2 6.52e1       3 4    
## 16 O4      low          3362.  2.11e3 8.91e2 7.22e2 4.32e+2 1.55e2       3 4    
## 17 A4      conventional  460.  1.47e1 1   e0 1.32e1 1.25e+0 4.25e0       4 4    
## 18 A4      low            10.7 8.25e0 9.25e0 2.5 e0 5   e-1 1.25e0       4 4    
## # … with abbreviated variable names ¹​Corn_weed_management, ²​phase_order
```


```
## `summarise()` has grouped output by 'Crop_ID', 'Corn_weed_management'. You can
## override using the `.groups` argument.
```

```{=html}
<template id="b8cff97f-ff6a-47ff-898d-7a9bb1713810"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}
</style><div class="tabwid"><style>.cl-108037d4{}.cl-10750fe4{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-107a675a{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-107a676e{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-107a7fce{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-107a7fd8{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-107a7fd9{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-107a7fe2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-107a7fe3{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-107a7fec{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-103530fe{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-108037d4'>

```

<caption>(\#tab:sim1-seed-prod-threshold)<span class="cl-103530fe">Seed production thresholds averaged over 10000 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seeds in the bottom 2 - 20 cm soil stratum.</span></caption>

```{=html}

<thead><tr style="overflow-wrap:break-word;"><td class="cl-107a7fce"><p class="cl-107a675a"><span class="cl-10750fe4">Crop ID</span></p></td><td class="cl-107a7fce"><p class="cl-107a675a"><span class="cl-10750fe4">Corn weed management</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 1</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 2</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 3</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 4</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 5</span></p></td><td class="cl-107a7fd8"><p class="cl-107a676e"><span class="cl-10750fe4">cohort 6</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C2</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">454.86</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,861.28</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3.27</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">20.45</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7.12</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.22</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C2</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,874.41</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">8,190.58</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">777.33</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,116.92</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">394.41</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">14.59</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S2</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,232.38</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">313.85</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">21.09</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,771.20</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">12.75</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">4.52</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S2</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">891.19</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">209.31</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">13.92</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,369.89</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1.86</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.48</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">106.98</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">544.28</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.59</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">6.49</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.63</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.04</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3,102.25</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7,784.20</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,896.20</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">646.98</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">52.25</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">18.85</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">221.86</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">45.48</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3.47</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.30</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.02</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.05</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">418.93</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">79.75</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">308.23</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">37.04</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">29.66</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">19.33</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">O3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">4,629.98</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,147.01</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3,435.82</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,122.19</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">74.96</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">10.20</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">O3</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,469.09</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,553.85</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">6,670.59</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,972.25</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">771.23</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">170.52</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">100.07</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">545.46</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.61</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1.71</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1.18</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.16</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">C4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3,224.09</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">8,401.50</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">612.16</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">313.26</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">82.43</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7.15</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">194.66</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">39.25</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.30</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">162.58</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">19.86</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">69.59</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">S4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">346.77</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">112.40</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">15.90</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7.76</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.30</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">83.03</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">O4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">3,752.11</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,928.32</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">2,590.26</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,262.50</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,252.16</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">21.70</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">O4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">16,086.05</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">24,621.11</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">36,657.54</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">18,593.19</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">9,482.49</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">1,602.32</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">A4</span></p></td><td class="cl-107a7fd9"><p class="cl-107a675a"><span class="cl-10750fe4">conventional</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7,271.06</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">520.98</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">0.30</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">356.69</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">8.13</span></p></td><td class="cl-107a7fe2"><p class="cl-107a676e"><span class="cl-10750fe4">7.60</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-107a7fe3"><p class="cl-107a675a"><span class="cl-10750fe4">A4</span></p></td><td class="cl-107a7fe3"><p class="cl-107a675a"><span class="cl-10750fe4">low</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">1,499.39</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">2,587.35</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">43.00</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">595.41</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">33.52</span></p></td><td class="cl-107a7fec"><p class="cl-107a676e"><span class="cl-10750fe4">32.12</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="02023fc3-98ed-4da9-a356-9732fd36ff75"></div>
<script>
var dest = document.getElementById("02023fc3-98ed-4da9-a356-9732fd36ff75");
var template = document.getElementById("b8cff97f-ff6a-47ff-898d-7a9bb1713810");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```



<div class="figure">
<img src="Q1-seed-production-allowance-rot_files/figure-html/annualized-lambda-against-manipulated-seed-production-1.png" alt="Cohort-based seed production threshold on natural logarithm scale for waterhemp population stabilization over 10000 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. It was expected that no waterhemp cohorts in any crop environments but only the cohorts 1 through 3 in corn and soybean were manipulated to find the seed production thresholds. However, additional control efficacy was needed in some crop phases outside of the expected group to meet annualized lambda = 1. The dots colored blue are where control measures extended beyond waterhemp cohort 3 would be necessary. The relationships of aboveground mass and fecundity in Nguyen and Liebman (2022a) were used to estimate per-capita seed production threshold in each cohort. The black horizontal lines mark annualized lambda = 1. The right-hand-side panel labels indicate the crop identities, which are the combinations of the first letter in crop species names and the rotation to which the crops belonged (C2, corn in the 2-year rotation; C3, corn in the 3-year rotation; C4, corn in the 4-year rotation; S2, soybean in the 2-year rotation; S3, soybean in the 3-year rotation; S4, soybean in the 4-year rotation; O3, oat in the 3-year rotation; O4, oat in the 4-year rotation; and A4, alfalfa in the 4-year rotation)."  />
<p class="caption">(\#fig:annualized-lambda-against-manipulated-seed-production)Cohort-based seed production threshold on natural logarithm scale for waterhemp population stabilization over 10000 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. It was expected that no waterhemp cohorts in any crop environments but only the cohorts 1 through 3 in corn and soybean were manipulated to find the seed production thresholds. However, additional control efficacy was needed in some crop phases outside of the expected group to meet annualized lambda = 1. The dots colored blue are where control measures extended beyond waterhemp cohort 3 would be necessary. The relationships of aboveground mass and fecundity in Nguyen and Liebman (2022a) were used to estimate per-capita seed production threshold in each cohort. The black horizontal lines mark annualized lambda = 1. The right-hand-side panel labels indicate the crop identities, which are the combinations of the first letter in crop species names and the rotation to which the crops belonged (C2, corn in the 2-year rotation; C3, corn in the 3-year rotation; C4, corn in the 4-year rotation; S2, soybean in the 2-year rotation; S3, soybean in the 3-year rotation; S4, soybean in the 4-year rotation; O3, oat in the 3-year rotation; O4, oat in the 4-year rotation; and A4, alfalfa in the 4-year rotation).</p>
</div>
   



