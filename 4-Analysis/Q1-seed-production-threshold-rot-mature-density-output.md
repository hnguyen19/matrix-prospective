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

The data in the model projection was used in this simulation. 100 iterations of simulation were run per each rotation crossed with corn weed management regime.  



























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
  

  seed_C[1,3] <- rlnorm(1, 2.66, 0.89) #13.58 seeds/plant
  seed_C[1,4] <- rlnorm(1, 3.44, 0.18)  # 30.84 seeds/plant
  seed_C[1,5] <- rlnorm(1, 3.44, 0.18) 


  seed_S[1,3] <- rlnorm(1, 3.44, 0.18)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 3.44, 0.18)


 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# seed at harvest
 l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
            sum(seed_dens_corn, seed_dens_soy))
 names(l) <- c("corn_first3", "corn_total",
               "soybean_first3", "soybean_total", "rotation_total")
 l
}

rot_2year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S){
  

  seed_C[1,3] <- rlnorm(1, 4.81, 0.56) # 122.33 seeds/plant
  seed_C[1,4] <- rlnorm(1,  5.05, 0.53) # 155.93 seeds/plant
  seed_C[1,5] <- rlnorm(1,  5.34, 0.5) # 208.18 seeds/plant

  seed_S[1,3] <- rlnorm(1, 4.81, 0.56)
  seed_S[1,4] <- rlnorm(1, 5.05, 0.53)
  seed_S[1,5] <- rlnorm(1, 5.34, 0.5)

 # corn phase dynamics   
   after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
   
   pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
   seed_dens_corn <- seed_C[1,3:8] * pl_dens_corn[3:8]
# soybean phase dynamics

  pl_dens_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
   
   seed_dens_soy <- seed_S[1,3:8] * pl_dens_soy[3:8] 

# seed at harvest
 l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
            sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
            sum(seed_dens_corn, seed_dens_soy))
 names(l) <- c("corn_first3", "corn_total",
               "soybean_first3", "soybean_total", "rotation_total")
 l
}
```


```r
##### with corn under conventional weed management {-}

m_dens_2yr_conv <- rot_2year_conv(vec = starting_point ,
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


m_dens_2yr_conv 
```

```
## $corn_first3
## [1] 224.5771
## 
## $corn_total
## [1] 231.3049
## 
## $soybean_first3
## [1] 2690.28
## 
## $soybean_total
## [1] 12326.13
## 
## $rotation_total
## [1] 12557.43
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
## [1] 5427.537
## 
## $corn_total
## [1] 5511.03
## 
## $soybean_first3
## [1] 5191.159
## 
## $soybean_total
## [1] 6791.261
## 
## $rotation_total
## [1] 12302.29
```
  



```r
rot_3year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O){
  

  seed_C[1,3] <- rlnorm(1, 5.55, 0.48) # 257.03 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.34, 0.5) # 208.18 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.34, 0.5)


  seed_S[1,3] <- rlnorm(1, 5.55, 0.48)
  seed_S[1,4] <- rlnorm(1, 5.34, 0.5)
  seed_S[1,5] <- rlnorm(1, 5.34, 0.5)

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
  l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), 
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
 names(l) <- c("corn_first3", "corn_total",
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
  

  seed_C[1,3] <- rlnorm(1, 5.75, 0.46) #316.83 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.55, 0.48) #257.03 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.55, 0.48)


  seed_S[1,3] <- rlnorm(1, 5.75, 0.46)
  seed_S[1,4] <- rlnorm(1, 5.55, 0.48)
  seed_S[1,5] <- rlnorm(1, 5.55, 0.48)

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
  l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), 
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat))
 names(l) <- c("corn_first3", "corn_total",
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
## $corn_first3
## [1] 8233.288
## 
## $corn_total
## [1] 8238.524
## 
## $soybean_first3
## [1] 29495.31
## 
## $soybean_total
## [1] 38826.81
## 
## $oat_total
## [1] 8875.672
## 
## $rotation_total
## [1] 55941.01
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
## [1] 29210.23
## 
## $corn_total
## [1] 29317.14
## 
## $soybean_first3
## [1] 35777.59
## 
## $soybean_total
## [1] 36440.32
## 
## $oat_total
## [1] 4843.788
## 
## $rotation_total
## [1] 70601.25
```





```r
### conventional weed management
rot_4year_conv <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  seed_C[1,3] <- rlnorm(1, 6, 0.45) #403.27 seeds/plant
  seed_C[1,4] <- rlnorm(1, 6, 0.45)
  seed_C[1,5] <- rlnorm(1, 6, 0.45)


  seed_S[1,3] <- rlnorm(1, 6, 0.45)
  seed_S[1,4] <- rlnorm(1, 6, 0.45)
  seed_S[1,5] <- rlnorm(1, 6, 0.45)

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
  l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), sum(seed_dens_alfalfa),
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
 names(l) <- c("corn_first3", "corn_total",
               "soybean_first3", "soybean_total", 
                "oat_total", "alfalfa_total",
               "rotation_total")
 l

}

### low herbicide weed management
rot_4year_low <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C,  seed_C, 
                           poh_S, ow_S, prt_S, em_S, sv_S, seed_S ,
                           poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  seed_C[1,3] <- rlnorm(1, 5.75, 0.46) #316.83 seeds/plant
  seed_C[1,4] <- rlnorm(1, 6, 0.45)
  seed_C[1,5] <- rlnorm(1, 6.63, 0.43) #757.22 seeds/plant


  seed_S[1,3] <- rlnorm(1, 5.75, 0.46)
  seed_S[1,4] <- rlnorm(1, 6, 0.45)
  seed_S[1,5] <- rlnorm(1, 6.63, 0.43)

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
  l <-  list(sum(seed_dens_corn[1:3]), sum(seed_dens_corn),
             sum(seed_dens_soy[1:3]), sum(seed_dens_soy),
             sum(seed_dens_oat), sum(seed_dens_alfalfa),
             sum(seed_dens_corn, seed_dens_soy, seed_dens_oat, seed_dens_alfalfa))
             
 names(l) <- c("corn_first3", "corn_total",
               "soybean_first3", "soybean_total", 
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
## $corn_first3
## [1] 40315.53
## 
## $corn_total
## [1] 40333.43
## 
## $soybean_first3
## [1] 4441.924
## 
## $soybean_total
## [1] 5179.546
## 
## $oat_total
## [1] 1037.449
## 
## $alfalfa_total
## [1] 16184.69
## 
## $rotation_total
## [1] 62735.12
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
## [1] 529203.5
## 
## $corn_total
## [1] 529605.7
## 
## $soybean_first3
## [1] 51184.65
## 
## $soybean_total
## [1] 83451.36
## 
## $oat_total
## [1] 27365.53
## 
## $alfalfa_total
## [1] 14681.58
## 
## $rotation_total
## [1] 655104.2
```

