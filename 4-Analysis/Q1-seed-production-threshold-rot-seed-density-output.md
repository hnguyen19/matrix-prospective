---
output:
 # bookdown::word_document2:
  bookdown::html_document2:
      toc: false
      fig_caption: yes
      keep_md: true
bibliography: WH-pop-dynamics.bib
csl: apa-no-ampersand.csl 
---
Goal: output the total seed production in each crop phase 


























```r
# event sequence: seed dropped - chisel - overwinter - field cultivator - emerge - survive - new seed

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
  

  seed_C[1,3] <- rlnorm(1, 5.55, 0.48) #257.03 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.34, 0.5) # 208.18 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.34, 0.5) 


  seed_S[1,3] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,4] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,5] <- rlnorm(1, 5.75, 0.46) #316.83


 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics

  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# seed at harvest
 l <-  list(pl_dens_corn[3:8], pl_dens_soy[3:8], 
             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
            sum(seed_dens_corn, seed_dens_soy))
 names(l) <- c("plant density in corn", "plant density in soybean",
               "corn_first3", "corn_total",
               "soybean_first3", "soybean_total", "rotation_total")
 l
}

rot_2year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S){
  

  seed_C[1,3] <- rlnorm(1, 3.85, 0.7) # 46.55 seeds/plant
  seed_C[1,4] <- rlnorm(1, 3.44, 0.76) # 30.84 seeds/plant
  seed_C[1,5] <- rlnorm(1, 3.85, 0.7)

  seed_S[1,3] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,4] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,5] <- rlnorm(1, 4.22, 0.65) #67.56 seeds/plant

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics

  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# seed at harvest
 l <-  list(pl_dens_corn[3:8], pl_dens_soy[3:8], 
             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
            sum(seed_dens_corn, seed_dens_soy))
 names(l) <- c("plant density in corn", "plant density in soybean",
               "corn_first3", "corn_total",
               "soybean_first3", "soybean_total", "rotation_total")
 l
}
```


```r
##### with corn under conventional weed management {-}
rot_2year_conv(vec = starting_point ,
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
```

```
## $`plant density in corn`
## [1] 0.9909086521 6.3079932836 0.0094328264 0.0007860689 0.0007860689
## [6] 0.0001965172
## 
## $`plant density in soybean`
## [1] 73.27376232 18.52714740  1.63211141  0.61303794  0.01288935  0.01689272
## 
## $corn_first3
## [1] 1463.188
## 
## $corn_total
## [1] 1469.915
## 
## $soybean_first3
## [1] 39842.35
## 
## $soybean_total
## [1] 51593.03
## 
## $rotation_total
## [1] 53062.95
```


 

 

```r
##### with corn under low herbicide weed management {-}
rot_2year_low(vec = starting_point ,
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
```

```
## $`plant density in corn`
## [1]  7.42427387 21.29697007  2.00331317  0.12069921  0.09627072  0.02291930
## 
## $`plant density in soybean`
## [1] 3.048677e+01 7.190798e+00 4.800203e-01 4.638054e-02 2.538704e-04
## [6] 2.538704e-04
## 
## $corn_first3
## [1] 1402.044
## 
## $corn_total
## [1] 1485.537
## 
## $soybean_first3
## [1] 1069.216
## 
## $soybean_total
## [1] 2077.833
## 
## $rotation_total
## [1] 3563.37
```
  



```r
rot_3year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O){
  

  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,6] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,7] <- rlnorm(1, 2.66, 0.89)
 # seed_C[1,8] <- rlnorm(1, 2.66, 0.89)

  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,6] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,7] <- rlnorm(1, 2.66, 0.89)
 # seed_S[1,8] <- rlnorm(1, 2.66, 0.89)

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics
after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%  after_corn

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  pl_dens_oat <-  sv_O %*% em_O %*% prt_O %*% after_soy 
   
   seed_dens_oat <- seed_O[1,3:8] * pl_dens_oat[3:8] 
   
   # seed at harvest
  l <-  list( pl_dens_corn[3:8],  pl_dens_soy[3:8], pl_dens_oat[3:8],
             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), 
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
 names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", 
               "corn_first3", "corn_total",
               "soybean_first3", "soybean_total", 
               "oat_total", "rotation_total")
 l
}

### low herbicide weed management
## Manipulation note: if cohorts 1 through 3 were reduced to rlnorm(1, 2.65, 0.89), alphas are around 0.5 --> super "safe", but hard
## cohorts 1 through 3 at rlnorm(1, 5.2, 0.51): more realistic
rot_3year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O){
  


  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 5.05, 0.53)


  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 6.94, 0.43)

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics
after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%  after_corn

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  pl_dens_oat <-  sv_O %*% em_O %*% prt_O %*% after_soy 
   
   seed_dens_oat <- seed_O[1,3:8] * pl_dens_oat[3:8] 
   
   # seed at harvest
  l <-  list( pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8],
             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), 
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
 names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", 
               "corn_first3", "corn_total",
               "soybean_first3", "soybean_total", 
               "oat_total", "rotation_total")
 l
}
```


```r
rot_3year_conv(vec = starting_point,
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
```

```
## $`plant density in corn`
## [1] 3.622802e+00 1.838942e+01 2.042593e-02 2.851070e-02 1.702161e-03
## [6] 4.255402e-04
## 
## $`plant density in soybean`
## [1] 42.299352137  8.441006491  0.649018551  0.055382685  0.004344372
## [6]  0.009533387
## 
## $`plant density in oat`
## [1] 0.079226274 0.145080531 0.464202894 0.295405458 0.034902791 0.007350662
## 
## $corn_first3
## [1] 862.3538
## 
## $corn_total
## [1] 862.915
## 
## $soybean_first3
## [1] 671.4894
## 
## $soybean_total
## [1] 1278.345
## 
## $oat_total
## [1] 655.3636
## 
## $rotation_total
## [1] 2796.624
```
 


```r
##### with corn under low herbicide weed management {-} 
 rot_3year_low(vec = starting_point,
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
```

```
## $`plant density in corn`
## [1] 21.77758473 54.16090090  5.13011444  0.66891402  0.21914216  0.05476482
## 
## $`plant density in soybean`
## [1] 42.042820107  8.253527497  0.594994190  0.002569026  0.002308562
## [6]  0.007486994
## 
## $`plant density in oat`
## [1] 0.11001119 0.19253972 0.63536318 0.42382630 0.14026389 0.04761262
## 
## $corn_first3
## [1] 4374.84
## 
## $corn_total
## [1] 4481.75
## 
## $soybean_first3
## [1] 1047.061
## 
## $soybean_total
## [1] 1234.4
## 
## $oat_total
## [1] 428.2556
## 
## $rotation_total
## [1] 6144.406
```





```r
### conventional weed management
rot_4year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  seed_C[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,5] <- rlnorm(1,  2.66, 0.89)
 #fecundity was much lower after cohort 3, so focus on supressing plant size in soybean


  seed_S[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,6] <- rlnorm(1,  7.34, 0.44)
  
 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics
after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%  after_corn

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  pl_dens_oat <-  sv_O %*% em_O %*% prt_O %*% after_soy 
   
   seed_dens_oat <- seed_O[1,3:8] * pl_dens_oat[3:8] 
# alfalfa phase dynamics
after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 

  pl_dens_alfalfa <-  sv_A %*% em_A %*% prt_A %*% after_oat 
   
   seed_dens_alfalfa <- seed_A[1,3:8] * pl_dens_alfalfa[3:8] 
      # seed at harvest
  l <-  list( pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8],
             sum(seed_dens_corn[1:3]), seed_dens_corn[4], sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), seed_dens_soy[4], sum(seed_dens_soy),
             sum(seed_dens_oat), sum(seed_dens_alfalfa),
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
 names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", "plant density in alfalfa",
   "corn_first3", "corn_4", "corn_total",
               "soybean_first3", "soybean_4",  "soybean_total", 
                "oat_total", "alfalfa_total",
               "rotation_total")
 l

}

### low herbicide weed management
rot_4year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  seed_C[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,6] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,7] <- rlnorm(1,  2.66, 0.89)
 # seed_C[1,8] <- rlnorm(1,  2.66, 0.89)


  seed_S[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,6] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,7] <- rlnorm(1,  2.66, 0.89)
 # seed_S[1,8] <- rlnorm(1,  2.66, 0.89)

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics
after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%  after_corn

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# oat phase dynamics
   after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
   
  pl_dens_oat <-  sv_O %*% em_O %*% prt_O %*% after_soy 
   
   seed_dens_oat <- seed_O[1,3:8] * pl_dens_oat[3:8] 
# alfalfa phase dynamics
after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 

  pl_dens_alfalfa <-  sv_A %*% em_A %*% prt_A %*% after_oat 
   
   seed_dens_alfalfa <- seed_A[1,3:8] * pl_dens_alfalfa[3:8] 
      # seed at harvest
  l <-  list( pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8],
             sum(seed_dens_corn[1:3]),  sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), seed_dens_soy[4], seed_dens_soy[5], sum(seed_dens_soy),
             sum(seed_dens_oat), sum(seed_dens_alfalfa),
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
 names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", "plant density in alfalfa",
   "corn_first3",  "corn_total",
               "soybean_first3", "soybean_4", "soybean_5", "soybean_total", 
                "oat_total", "alfalfa_total",
               "rotation_total")
 l
}
```


```r
##### with corn under conventional weed management {-}
 rot_4year_conv(vec = starting_point,
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
```

```
## $`plant density in corn`
## [1] 2.820161e+01 1.508272e+02 1.664582e-01 1.387152e-02 1.387152e-02
## [6] 3.467879e-03
## 
## $`plant density in soybean`
## [1] 1.9224968003 0.3889087811 0.0029872016 0.0201451862 0.0007748149
## [6] 0.0007748149
## 
## $`plant density in oat`
## [1] 0.016851651 0.047616432 0.161063340 0.092335335 0.057268473 0.005521709
## 
## $`plant density in alfalfa`
## [1]  5.9159586 13.3041727  0.1129969 10.0826801  2.4348098  0.6697810
## 
## $corn_first3
## [1] 5263.141
## 
## $corn_4
## [1] 10.01523
## 
## $corn_total
## [1] 5281.046
## 
## $soybean_first3
## [1] 241.6609
## 
## $soybean_4
## [1] 25.93936
## 
## $soybean_total
## [1] 286.4521
## 
## $oat_total
## [1] 196.0222
## 
## $alfalfa_total
## [1] 3058.039
## 
## $rotation_total
## [1] 8821.56
```
 


```r
##### with corn under low herbicide weed management {-} 
rot_4year_low(vec = starting_point,
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
```

```
## $`plant density in corn`
## [1] 149.6677626 395.4636340  29.2113672   1.8984729   1.5752202   0.3809348
## 
## $`plant density in soybean`
## [1] 1.771563343 0.560449649 0.080767156 0.038959500 0.001498442 0.001901062
## 
## $`plant density in oat`
## [1] 0.01923120 0.04690691 0.16524914 0.10342207 0.08816269 0.04161179
## 
## $`plant density in alfalfa`
## [1]  6.7717813 15.1083556  0.2239443 11.4733483  3.2295596  1.2379696
## 
## $corn_first3
## [1] 7360.635
## 
## $corn_total
## [1] 7424.27
## 
## $soybean_first3
## [1] 89.20542
## 
## $soybean_4
## [1] 0.3543871
## 
## $soybean_5
## [1] 0.03554655
## 
## $soybean_total
## [1] 98.48007
## 
## $oat_total
## [1] 430.1841
## 
## $alfalfa_total
## [1] 230.7934
## 
## $rotation_total
## [1] 8183.728
```

