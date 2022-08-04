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
   
   list(seed_dens_corn, seed_dens_soy)
# l <-  list(pl_dens_corn[3:8], pl_dens_soy[3:8], 
#             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
#            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
#            sum(seed_dens_corn, seed_dens_soy))
# names(l) <- c("plant density in corn", "plant density in soybean",
#               "corn_first3", "corn_total",
#               "soybean_first3", "soybean_total", "rotation_total")
# l
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
   list(seed_dens_corn, seed_dens_soy)
# l <-  list(pl_dens_corn[3:8], pl_dens_soy[3:8], 
#             sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
#            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
#            sum(seed_dens_corn, seed_dens_soy))
# names(l) <- c("plant density in corn", "plant density in soybean",
#               "corn_first3", "corn_total",
#               "soybean_first3", "soybean_total", "rotation_total")
# l
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
## [[1]]
## [1] 492.89638390 967.21162472   3.07962884   4.95113334   1.72316776
## [6]   0.05345268
## 
## [[2]]
## [1] 33306.21558  5931.72197   604.41483 11637.23103    83.77937    29.66737
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
## [[1]]
## [1] 253.6610125 700.6963785 447.6864435  61.1140324  21.5806858   0.7983557
## 
## [[2]]
## [1]  790.8589571  195.4211770   82.9355282 1006.8984353    1.3642151
## [6]    0.3548262
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
   
  list(seed_dens_corn, seed_dens_soy, seed_dens_oat) 
#  l <-  list( pl_dens_corn[3:8],  pl_dens_soy[3:8], pl_dens_oat[3:8],
#             sum(seed_dens_corn[1:3]), seed_dens_corn[4], seed_dens_corn[5], sum(seed_dens_corn),
#             sum(seed_dens_soy[1:3]), seed_dens_soy[4], seed_dens_soy[5], sum(seed_dens_soy),
#             sum(seed_dens_oat), 
#             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
# names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", 
#               "corn_first3", "corn_4", "corn_5", "corn_total",
#               "soybean_first3", "soybean_4", "soybean_5" ,"soybean_total", 
#               "oat_total", "rotation_total")
# l
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
  list(seed_dens_corn, seed_dens_soy, seed_dens_oat) 
#  l <-  list( pl_dens_corn[3:8],  pl_dens_soy[3:8], pl_dens_oat[3:8],
#             sum(seed_dens_corn[1:3]), seed_dens_corn[4], seed_dens_corn[5], sum(seed_dens_corn),
#             sum(seed_dens_soy[1:3]), seed_dens_soy[4], seed_dens_soy[5], sum(seed_dens_soy),
#             sum(seed_dens_oat), 
#             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
# names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", 
#               "corn_first3", "corn_4", "corn_5", "corn_total",
#               "soybean_first3", "soybean_4", "soybean_5" ,"soybean_total", 
#               "oat_total", "rotation_total")
# l
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
## [[1]]
## [1] 7.257569e+01 7.896607e+02 1.174527e-01 5.203554e-01 8.858695e-03
## [6] 3.202190e-02
## 
## [[2]]
## [1] 531.11959801 119.56709602  20.80271097   0.36275504   0.02292054
## [6] 606.47023889
## 
## [[3]]
## [1] 265.6985135 123.2096410 197.1701791  64.3983898   4.3017690   0.5851127
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
## [[1]]
## [1]  244.521178 3759.015789  371.303457   96.323619    7.779547    2.806697
## 
## [[2]]
## [1] 193.60019 144.46615 708.99512  80.66199  64.58587  42.09105
## 
## [[3]]
## [1]  72.387366  45.554899 195.564787  87.138687  22.610538   4.999325
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
   list(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
   
#  l <-  list( pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8],
#             sum(seed_dens_corn[1:3]), seed_dens_corn[4], sum(seed_dens_corn),
#             sum(seed_dens_soy[1:3]), seed_dens_soy[4], sum(seed_dens_soy),
#             sum(seed_dens_oat), sum(seed_dens_alfalfa),
#             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
# names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", "plant density in alfalfa",
#   "corn_first3", "corn_4", "corn_total",
#               "soybean_first3", "soybean_4",  "soybean_total", 
#                "oat_total", "alfalfa_total",
#               "rotation_total")
# l

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
   list(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
   
#  l <-  list( pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8],
#             sum(seed_dens_corn[1:3]), seed_dens_corn[4], sum(seed_dens_corn),
#             sum(seed_dens_soy[1:3]), seed_dens_soy[4], sum(seed_dens_soy),
#             sum(seed_dens_oat), sum(seed_dens_alfalfa),
#             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
# names(l) <- c("plant density in corn", "plant density in soybean", "plant density in oat", "plant density in alfalfa",
#   "corn_first3", "corn_4", "corn_total",
#               "soybean_first3", "soybean_4",  "soybean_total", 
#                "oat_total", "alfalfa_total",
#               "rotation_total")
# l
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
## [[1]]
## [1]  234.010819 5027.561563    1.568298   10.015235    6.945006    0.945575
## 
## [[2]]
## [1] 239.0542344   2.4952351   0.1114405  25.9393581   4.1854207  14.6664058
## 
## [[3]]
## [1] 62.2930634 48.6163767 43.0039118 20.9601210 20.7884556  0.3602915
## 
## [[4]]
## [1] 2723.3129386  195.1278666    0.1129969  133.5955117    3.0435122
## [6]    2.8465693
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
## [[1]]
## [1] 1646.437865 5566.572331  147.624913    5.115332   51.377529    7.142528
## 
## [[2]]
## [1] 70.90251839 17.71018712  0.59271588  0.35438713  0.03554655  8.88471827
## 
## [[3]]
## [1]  64.646749  98.947523 147.319612  74.722445  38.108325   6.439424
## 
## [[4]]
## [1]  72.232333 124.643934   2.071485  28.683371   1.614780   1.547462
```

