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

N_2yr_conv_lambda_df <- N_2yr_conv_lambda %>% 
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
N_2yr_low_lambda <- list() # blank dataframe to save loop output 

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
N_2yr_conv_manipulated <- rot_2year_conv_manipulated_outputs(vec = starting_point,
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



N_2yr_conv_manipulated_df <- N_2yr_conv_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 6)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)

### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_2yr_low_manipulated <- rot_2year_low_manipulated_outputs(vec = starting_point,
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


N_2yr_low_manipulated_df <- N_2yr_low_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 6)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```




```r
N_2yr_conv_original <- rot_2year_original_outputs(vec = starting_point,
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



N_2yr_conv_original_df <- N_2yr_conv_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(original_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 6)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = original_output, names_from = variable)

### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_2yr_low_original <- rot_2year_original_outputs(vec = starting_point,
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


N_2yr_low_original_df <- N_2yr_low_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 6)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```



```r
N_2yr_conv_all <- N_2yr_conv_lambda_df %>%
  left_join(N_2yr_conv_manipulated_df) %>%
  left_join(N_2yr_conv_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
```

```r
N_2yr_low_all <- N_2yr_low_lambda_df %>% 
  left_join(N_2yr_low_manipulated_df) %>%
  left_join(N_2yr_low_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
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
N_3yr_conv_manipulated <- rot_3year_conv_manipulated_outputs(vec = starting_point,
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



N_3yr_conv_manipulated_df <- N_3yr_conv_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 9)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)

### Output: Survival rate,  mature plant densities until seed production and seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_3yr_low_manipulated <- rot_3year_low_manipulated_outputs(vec = starting_point,
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


N_3yr_low_manipulated_df <- N_3yr_low_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 9)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```



```r
N_3yr_conv_original <- rot_3year_original_outputs(vec = starting_point,
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


N_3yr_conv_original_df <- N_3yr_conv_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(original_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 9)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = original_output, names_from = variable)

### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_3yr_low_original <- rot_3year_original_outputs(vec = starting_point,
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


N_3yr_low_original_df <- N_3yr_low_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 9)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```



```r
N_3yr_conv_all <- N_3yr_conv_lambda_df %>%
  left_join(N_3yr_conv_manipulated_df) %>%
  left_join(N_3yr_conv_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
```

```r
N_3yr_low_all <- N_3yr_low_lambda_df %>% 
  left_join(N_3yr_low_manipulated_df) %>%
  left_join(N_3yr_low_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
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
N_4yr_conv_manipulated <- rot_4year_conv_manipulated_outputs(vec = starting_point,
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



N_4yr_conv_manipulated_df <- N_4yr_conv_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 12)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)

### Output: Survival rate,  mature plant densities until seed production and seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_4yr_low_manipulated <- rot_4year_low_manipulated_outputs(vec = starting_point,
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


N_4yr_low_manipulated_df <- N_4yr_low_manipulated %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 12)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```



```r
N_4yr_conv_original <- rot_4year_original_outputs(vec = starting_point,
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



N_4yr_conv_original_df <- N_4yr_conv_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(original_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 12)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "conventional",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = original_output, names_from = variable)

### Output: Mature plant densities until seed production (B_h = sv_C or sv_S)
### 1 iteration only because no randomization at any matrix

N_4yr_low_original <- rot_4year_original_outputs(vec = starting_point,
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


N_4yr_low_original_df <- N_4yr_low_original %>% 
  unlist(recursive = TRUE) %>% # make a long table
  data.frame() %>%
  tibble::rownames_to_column(., "variable") %>%
  dplyr::rename(manipulated_output = ".")  %>%
  dplyr::mutate(cohort = rep(c("1", "2", "3", "4", "5", "6"), 12)) %>%
   mutate(Crop_ID = substr(variable, 1, 2),
          Corn_weed_management = "low",
          variable = substr(variable, 4, 19)) %>%
  pivot_wider(values_from = manipulated_output, names_from = variable)
```



```r
N_4yr_conv_all <- N_4yr_conv_lambda_df %>%
  left_join(N_4yr_conv_manipulated_df) %>%
  left_join(N_4yr_conv_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
```

```r
N_4yr_low_all <- N_4yr_low_lambda_df %>% 
  left_join(N_4yr_low_manipulated_df) %>%
  left_join(N_4yr_low_original_df)
```

```
## Joining, by = "Corn_weed_management"
## Joining, by = c("Corn_weed_management", "cohort", "Crop_ID")
```





```r
mature_plant_allowance_sim_df %>%
  mutate(plant_control_efficacy = 1 - manipulated_surv) %>%
  group_by(Crop_ID, Corn_weed_management, cohort) %>%
  summarise(mean_efficacy = mean(plant_control_efficacy)) %>%
  pivot_wider(names_from = cohort, values_from = mean_efficacy) %>%
    mutate(phase_order = ifelse(str_detect(Crop_ID, "C"), 1,
                              ifelse(str_detect(Crop_ID, "S"), 2,
                                     ifelse(str_detect(Crop_ID, "O"), 3, 4)))) %>%
  mutate(Rot = substr(Crop_ID,2,2))%>%
  arrange(Rot, phase_order, Corn_weed_management) %>%
    mutate_at(vars(`1`:`6`), funs(round(., 2))) %>%
  select(Crop_ID, Corn_weed_management, `1`:`6`) %>%
  flextable() %>%
    set_caption("Necessary control efficacy (with respect to seedling survival to maturity) averaged over 100 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seeds in the bottom 2 - 20 cm soil stratum.")  %>%
  set_header_labels(values = list(Crop_ID = "Crop ID",
                                  Corn_weed_management = "Corn weed management",
               `1` = "cohort 1",
               `2` = "cohort 2",
               `3` = "cohort 2",
               `4` = "cohort 4",
               `5` = "cohort 5",
               `6` = "cohort 6"))
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
<template id="6d064291-6b12-4405-ab8b-5f439131064b"><style>
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
</style><div class="tabwid"><style>.cl-f8b478d6{}.cl-f8a65346{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f8ab6dd6{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f8ab6de0{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-f8ab8c44{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f8ab8c4e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f8ab8c4f{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f8ab8c58{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f8ab8c59{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f8ab8c62{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f897eb94{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}</style><table class='cl-f8b478d6'>

```

<caption>(\#tab:mature-plant-threshold-tab)<span class="cl-f897eb94">Necessary control efficacy (with respect to seedling survival to maturity) averaged over 100 rotational cycles (the 2-year rotation cycled over two years and ended at the soybean phase, the 3-year rotation cycled over three years and ended at the oat phase, and the 4-year rotation cycled over four years and ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seeds in the bottom 2 - 20 cm soil stratum.</span></caption>

```{=html}

<thead><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c44"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">Crop ID</span></p></td><td class="cl-f8ab8c44"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">Corn weed management</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 1</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 2</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 2</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 4</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 5</span></p></td><td class="cl-f8ab8c4e"><p class="cl-f8ab6de0"><span class="cl-f8a65346">cohort 6</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C2</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C2</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S2</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.74</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S2</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.74</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.74</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">O3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">O3</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">C4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.95</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.96</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">S4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">1.00</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.95</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.99</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">O4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">O4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.10</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">A4</span></p></td><td class="cl-f8ab8c4f"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">conventional</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c58"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-f8ab8c59"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">A4</span></p></td><td class="cl-f8ab8c59"><p class="cl-f8ab6dd6"><span class="cl-f8a65346">low</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.90</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td><td class="cl-f8ab8c62"><p class="cl-f8ab6de0"><span class="cl-f8a65346">0.50</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="7ad20c72-51e4-4a9d-b36b-edae4931acf1"></div>
<script>
var dest = document.getElementById("7ad20c72-51e4-4a9d-b36b-edae4931acf1");
var template = document.getElementById("6d064291-6b12-4405-ab8b-5f439131064b");
var caption = template.content.querySelector("caption");
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

```


<div class="figure">
<img src="Q2-mature-density-allowance-rot_files/figure-html/annualized-lambda-against-manipulated-survival-1.png" alt="Cohort-based mature plant density thresholds on natural logarithm scale for waterhemp population stabilization averaged over 100 rotational cycles (the 2-year rotation ended at the soybean phase, the 3-year rotation ended at the oat phase, and the 4-year rotation ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. The simulation applied improved control efficacy on waterhemp cohorts 1 through 3 in corn and soybean only. The relationships of aboveground mass and fecundity in Nguyen and Liebman (2022a) were used to estimate cohort-based fecundity. It was expected that no waterhemp cohorts in any crop environments but only the cohorts 1 through 3 in corn and soybean had their survival rates manipulated to find the mature plant density thresholds for which annualized lambda = 1. However, additional control efficacy was needed in some crop phases outside of the expected groups to reduced the mature plant densities. The black horizontal lines mark lambda = 1. The right-hand-side panel labels indicate the crop identities, which are the combinations of the first letter in crop species names and the rotation to which the crops belonged (C2, corn in the 2-year rotation; C3, corn in the 3-year rotation; C4, corn in the 4-year rotation; S2, soybean in the 2-year rotation; S3, soybean in the 3-year rotation; S4, soybean in the 4-year rotation; O3, oat in the 3-year rotation; O4, oat in the 4-year rotation; and A4, alfalfa in the 4-year rotation)."  />
<p class="caption">(\#fig:annualized-lambda-against-manipulated-survival)Cohort-based mature plant density thresholds on natural logarithm scale for waterhemp population stabilization averaged over 100 rotational cycles (the 2-year rotation ended at the soybean phase, the 3-year rotation ended at the oat phase, and the 4-year rotation ended at the alfalfa phase). All simulations started with a seed column of 10000 female seeds in the top 0 - 2 cm soil stratum and 0 female seed in the bottom 2 - 20 cm soil stratum. The simulation applied improved control efficacy on waterhemp cohorts 1 through 3 in corn and soybean only. The relationships of aboveground mass and fecundity in Nguyen and Liebman (2022a) were used to estimate cohort-based fecundity. It was expected that no waterhemp cohorts in any crop environments but only the cohorts 1 through 3 in corn and soybean had their survival rates manipulated to find the mature plant density thresholds for which annualized lambda = 1. However, additional control efficacy was needed in some crop phases outside of the expected groups to reduced the mature plant densities. The black horizontal lines mark lambda = 1. The right-hand-side panel labels indicate the crop identities, which are the combinations of the first letter in crop species names and the rotation to which the crops belonged (C2, corn in the 2-year rotation; C3, corn in the 3-year rotation; C4, corn in the 4-year rotation; S2, soybean in the 2-year rotation; S3, soybean in the 3-year rotation; S4, soybean in the 4-year rotation; O3, oat in the 3-year rotation; O4, oat in the 4-year rotation; and A4, alfalfa in the 4-year rotation).</p>
</div>

