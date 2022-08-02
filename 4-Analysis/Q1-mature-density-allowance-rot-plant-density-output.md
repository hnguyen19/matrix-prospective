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

Goal: output the mature plant density in each crop phase 



























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
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 
  
  # seed at harvest
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             seed_dens_corn, seed_dens_soy)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                "seed production in corn", "seed production in soybean")
  l
}

rot_2year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                          poh_S, ow_S, prt_S, em_S, sv_S, seed_S){
  
  
  sv_C[3,3] <- .001 #99.9 % efficacy
  sv_C[4,4] <- .002
  sv_C[5,5] <- .003 # 99.8% efficacy




  sv_S[3,3] <- .001
  sv_S[4,4] <-  .002
  sv_S[5,5] <-  .003

  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 
  
  # seed at harvest
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             seed_dens_corn, seed_dens_soy)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                "seed production in corn", "seed production in soybean")
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
## $corn_first3
## [1] 0.08604575
## 
## $corn_total
## [1] 0.0878144
## 
## $soybean_first3
## [1] 0.6956457
## 
## $soybean_total
## [1] 1.340562
## 
## $`seed production in corn`
## [1] 2.553524e+02 1.229454e+03 8.205144e-01 4.951133e+00 1.723168e+00
## [6] 5.345268e-02
## 
## $`seed production in soybean`
## [1] 24798.10075  4852.28153   943.29366 11675.17483    84.05254    29.76410
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
## $corn_first3
## [1] 0.1100965
## 
## $corn_total
## [1] 0.3499857
## 
## $soybean_first3
## [1] 0.05577889
## 
## $soybean_total
## [1] 0.1001423
## 
## $`seed production in corn`
## [1] 743.6682036 277.5014920  11.2394216  61.1140324  21.5806858   0.7983557
## 
## $`seed production in soybean`
## [1] 1879.6257032 1278.0051666  289.1644169  952.6788502    1.2907547
## [6]    0.3357195
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
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             sum(pl_dens_oat[6:8]), sum(pl_dens_oat[3:8]),
             seed_dens_corn, seed_dens_soy, seed_dens_oat)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                "oat_last3", "oat_total",
                "seed production in corn", "seed production in soybean", "seed production in oat")
  l
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




  sv_S[3,3] <- .0001
  sv_S[4,4] <- .003
  sv_S[5,5] <- .003


  
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
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             sum(pl_dens_oat[6:8]), sum(pl_dens_oat[3:8]),
             seed_dens_corn, seed_dens_soy, seed_dens_oat)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                "oat_last3", "oat_total",
                "seed production in corn", "seed production in soybean", "seed production in oat")
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
## $corn_first3
## [1] 0.00426667
## 
## $corn_total
## [1] 0.004346457
## 
## $soybean_first3
## [1] 0.005462622
## 
## $soybean_total
## [1] 0.005601641
## 
## $oat_last3
## [1] 0.01579295
## 
## $oat_total
## [1] 0.5942385
## 
## $`seed production in corn`
## [1]  9.342803760 30.517917971  0.013356762  0.011855697  0.001154632
## [6]  0.000320219
## 
## $`seed production in soybean`
## [1] 5463.189642  613.031819   54.436923    1.152075    1.970549    5.266841
## 
## $`seed production in oat`
## [1] 657.95428574 305.10637889   9.76512534   3.18941916   0.11836169
## [6]   0.01609917
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
## $corn_first3
## [1] 0.3731614
## 
## $corn_total
## [1] 1.315982
## 
## $soybean_first3
## [1] 0.02363582
## 
## $soybean_total
## [1] 0.03070393
## 
## $oat_last3
## [1] 1.05207
## 
## $oat_total
## [1] 2.665192
## 
## $`seed production in corn`
## [1]   0.7578945 175.1202462   6.6451014  96.3236191   7.7795465   2.8066968
## 
## $`seed production in soybean`
## [1] 1952.05267 2755.50237  439.32626   46.10972   36.91995   24.06098
## 
## $`seed production in oat`
## [1] 124.499318  78.350051 336.352654 149.870173  38.887955   8.598358
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
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             sum(pl_dens_oat[3:8]), sum(pl_dens_alfalfa[3:8]),
             seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                 "oat_total", "alfalfa_total",
                "seed production in corn", "seed production in soybean",
                "seed production in oat", "seed production in alfalfa")
  l
  
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
  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
             sum(pl_dens_oat[3:8]), sum(pl_dens_alfalfa[3:8]),
             seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
  names(l) <- c("corn_first3", "corn_total",
                "soybean_first3", "soybean_total", 
                 "oat_total", "alfalfa_total",
                "seed production in corn", "seed production in soybean",
                "seed production in oat", "seed production in alfalfa")
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
## $corn_first3
## [1] 0.3179274
## 
## $corn_total
## [1] 0.3231292
## 
## $soybean_first3
## [1] 0.000520731
## 
## $soybean_total
## [1] 0.001381198
## 
## $oat_total
## [1] 0.2752221
## 
## $alfalfa_total
## [1] 23.51286
## 
## $`seed production in corn`
## [1]  314.4893817 2238.5266107    6.2513143    1.2519043    0.8681257
## [6]    0.4727875
## 
## $`seed production in soybean`
## [1] 48.6799673 34.9429964  0.3257924  2.1475835  1.5493674  5.4292392
## 
## $`seed production in oat`
## [1] 45.0390573 35.1505554 31.0926376 15.1545620 15.0304446  0.2604976
## 
## $`seed production in alfalfa`
## [1] 1.969007e+03 1.410811e+02 8.169891e-02 9.659207e+01 2.200517e+00
## [6] 2.058123e+00
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
## $corn_first3
## [1] 1.389413
## 
## $corn_total
## [1] 5.718659
## 
## $soybean_first3
## [1] 0.002320538
## 
## $soybean_total
## [1] 0.00920007
## 
## $oat_total
## [1] 0.3167994
## 
## $alfalfa_total
## [1] 25.94284
## 
## $`seed production in corn`
## [1]  109.392088 1935.300159  454.875758  390.966768   82.305255    7.142528
## 
## $`seed production in soybean`
## [1]  5.482964  6.203371 44.604916 86.260440  6.083692  5.611853
## 
## $`seed production in oat`
## [1]  44.08258  67.47226 100.45716  50.95319  25.98605   4.39104
## 
## $`seed production in alfalfa`
## [1] 49.255188 84.994630  1.412544 19.559175  1.101117  1.055214
```

