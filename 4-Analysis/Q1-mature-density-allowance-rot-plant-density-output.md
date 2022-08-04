---
output:
  bookdown::word_document2:
 # bookdown::html_document2:
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
  
  list(pl_dens_corn, pl_dens_soy )
  
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             seed_dens_corn, seed_dens_soy)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                "seed production in corn", "seed production in soybean")
#  l
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
  
  
    list(pl_dens_corn , pl_dens_soy )
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             seed_dens_corn, seed_dens_soy)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                "seed production in corn", "seed production in soybean")
#  l

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
##              [,1]
## [1,] 3.902680e+03
## [2,] 2.986379e+03
## [3,] 7.077919e-03
## [4,] 7.884992e-02
## [5,] 1.179103e-04
## [6,] 7.860689e-04
## [7,] 7.860689e-04
## [8,] 1.965172e-04
## 
## [[2]]
##              [,1]
## [1,] 1.172918e+03
## [2,] 2.945745e+03
## [3,] 5.250905e-01
## [4,] 1.327683e-01
## [5,] 3.778692e-02
## [6,] 6.150368e-01
## [7,] 1.293138e-02
## [8,] 1.694780e-02
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
##              [,1]
## [1,] 3.869338e+03
## [2,] 2.986379e+03
## [3,] 8.838421e-03
## [4,] 8.873738e-02
## [5,] 1.252071e-02
## [6,] 1.206992e-01
## [7,] 9.627072e-02
## [8,] 2.291930e-02
## 
## [[2]]
##              [,1]
## [1,] 1.149138e+03
## [2,] 2.820558e+03
## [3,] 3.433943e-02
## [4,] 1.619902e-02
## [5,] 5.240448e-03
## [6,] 4.388304e-02
## [7,] 2.402000e-04
## [8,] 2.402000e-04
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
  
   list(pl_dens_corn , pl_dens_soy, pl_dens_oat)
  
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             sum(pl_dens_oat[6:8]), sum(pl_dens_oat[3:8]),
#             seed_dens_corn, seed_dens_soy, seed_dens_oat)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                "oat_last3", "oat_total",
#                "seed production in corn", "seed production in soybean", "seed production in oat")
#  l
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
  
     list(pl_dens_corn , pl_dens_soy, pl_dens_oat) 
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             sum(pl_dens_oat[6:8]), sum(pl_dens_oat[3:8]),
#             seed_dens_corn, seed_dens_soy, seed_dens_oat)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                "oat_last3", "oat_total",
#                "seed production in corn", "seed production in soybean", "seed production in oat")
#  l
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
##              [,1]
## [1,] 3.883565e+03
## [2,] 2.986379e+03
## [3,] 4.312859e-04
## [4,] 3.831128e-03
## [5,] 4.255402e-06
## [6,] 7.127674e-05
## [7,] 4.255402e-06
## [8,] 4.255402e-06
## 
## [[2]]
##              [,1]
## [1,] 9.488609e+02
## [2,] 2.496192e+03
## [3,] 4.373158e-03
## [4,] 8.726814e-04
## [5,] 2.167826e-04
## [6,] 1.849871e-05
## [7,] 3.772835e-05
## [8,] 8.279193e-05
## 
## [[3]]
##              [,1]
## [1,] 1.998824e+03
## [2,] 2.545056e+03
## [3,] 1.961895e-01
## [4,] 3.592657e-01
## [5,] 2.299029e-02
## [6,] 1.463036e-02
## [7,] 9.603383e-04
## [8,] 2.022509e-04
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
##              [,1]
## [1,] 3.795660e+03
## [2,] 2.986379e+03
## [3,] 2.592570e-03
## [4,] 3.385056e-01
## [5,] 3.206322e-02
## [6,] 6.689140e-01
## [7,] 2.191422e-01
## [8,] 5.476482e-02
## 
## [[2]]
##              [,1]
## [1,] 9.912845e+02
## [2,] 2.545572e+03
## [3,] 2.861120e-03
## [4,] 1.685020e-02
## [5,] 3.924499e-03
## [6,] 1.468561e-03
## [7,] 1.319669e-03
## [8,] 4.279874e-03
## 
## [[3]]
##              [,1]
## [1,] 1.785758e+03
## [2,] 2.402365e+03
## [3,] 1.892087e-01
## [4,] 3.311498e-01
## [5,] 1.092764e+00
## [6,] 7.289405e-01
## [7,] 2.412404e-01
## [8,] 8.188913e-02
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
  
     list(pl_dens_corn , pl_dens_soy, pl_dens_oat, pl_dens_alfalfa)
  
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             sum(pl_dens_oat[3:8]), sum(pl_dens_alfalfa[3:8]),
#             seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                 "oat_total", "alfalfa_total",
#                "seed production in corn", "seed production in soybean",
#                "seed production in oat", "seed production in alfalfa")
#  l
  
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

  list(pl_dens_corn , pl_dens_soy, pl_dens_oat, pl_dens_alfalfa)
  
  # seed at harvest
#  l <-  list(sum(pl_dens_corn[3:5]), sum(pl_dens_corn[3:8]),
#             sum(pl_dens_soy[3:5]), sum(pl_dens_soy[3:8]),
#             sum(pl_dens_oat[3:8]), sum(pl_dens_alfalfa[3:8]),
#             seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa)
#  names(l) <- c("corn_first3", "corn_total",
#                "soybean_first3", "soybean_total", 
#                 "oat_total", "alfalfa_total",
#                "seed production in corn", "seed production in soybean",
#                "seed production in oat", "seed production in alfalfa")
#  l
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
##              [,1]
## [1,] 3.682504e+03
## [2,] 2.986379e+03
## [3,] 3.357335e-03
## [4,] 3.142233e-01
## [5,] 3.467879e-04
## [6,] 1.733939e-03
## [7,] 1.733939e-03
## [8,] 1.733939e-03
## 
## [[2]]
##              [,1]
## [1,] 1.405324e+03
## [2,] 3.205207e+03
## [3,] 1.694461e-04
## [4,] 3.427786e-04
## [5,] 8.506217e-06
## [6,] 2.868225e-04
## [7,] 2.868225e-04
## [8,] 2.868225e-04
## 
## [[3]]
##              [,1]
## [1,] 6.178561e+02
## [2,] 1.774744e+03
## [3,] 1.218406e-02
## [4,] 3.442758e-02
## [5,] 1.164518e-01
## [6,] 6.676019e-02
## [7,] 4.140618e-02
## [8,] 3.992300e-03
## 
## [[4]]
##              [,1]
## [1,] 185.71110213
## [2,] 964.28474241
## [3,]   4.27734941
## [4,]   9.61916729
## [5,]   0.08169891
## [6,]   7.28996751
## [7,]   1.76041328
## [8,]   0.48426428
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
##              [,1]
## [1,] 3.130943e+03
## [2,] 2.986379e+03
## [3,] 1.781759e-02
## [4,] 8.238826e-01
## [5,] 5.477131e-01
## [6,] 2.373091e+00
## [7,] 1.575220e+00
## [8,] 3.809348e-01
## 
## [[2]]
##              [,1]
## [1,] 1.381087e+03
## [2,] 3.164443e+03
## [3,] 1.332110e-04
## [4,] 4.214246e-04
## [5,] 1.765902e-03
## [6,] 4.732304e-03
## [7,] 9.464608e-04
## [8,] 1.200767e-03
## 
## [[3]]
##              [,1]
## [1,] 6.243753e+02
## [2,] 1.763170e+03
## [3,] 1.311375e-02
## [4,] 3.198580e-02
## [5,] 1.126833e-01
## [6,] 7.052345e-02
## [7,] 6.011809e-02
## [8,] 2.837505e-02
## 
## [[4]]
##             [,1]
## [1,] 242.3445075
## [2,] 957.9959509
## [3,]   4.6176739
## [4,]  10.3023794
## [5,]   0.1527075
## [6,]   7.8236699
## [7,]   2.2022349
## [8,]   0.8441708
```

