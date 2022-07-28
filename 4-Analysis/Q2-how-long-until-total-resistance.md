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


 

The available models for herbicide resistance profile estimation do not accommodate populations with depth-structured seedbanks and cohort-structured plants that are controlled with herbicide mixtures [@gresselModellingEffectivenessHerbicide1990a; @maxwellPredictingEvolutionDynamics1990; @cousensDynamicsWeedPopulations1995]. Since the waterhemp populations in this study might be resistant to multiple herbicide modes of action, a generalized equation, similar to equation 7 in @liuGeneralisedIndividualbasedAlgorithm2017 is used to calculate the proportion of resistant seeds at the end of each rotation cycle.


$$
R_e = \frac{R_{seedbank} + R_{fresh \, seeds}}{N_{seedbank} + N_{fresh \, seeds}}
$$
where,  

$R_e$ is the proportion of resistant seeds at the end of a rotation cycle,  
$N_{seedbank}$ is the total number of dormant seeds in the soil seedbank,  
$N_{fresh \, seeds}$ is the total seed production in a rotation cycle,    
$R_{seedbank}$ is the total number of resistant seeds in the soil seedbank, and  
$R_{fresh \, seeds}$ is the the number of resistant seeds produced at the end of the rotation cycle.  

As the projection model only keeps track of plump, viable seeds, it is unnecessary to subtract the non-viable seeds in this equation. As close to 90% of the mature seeds germinated after either 30 C or -20 C post-harvest treatment [Figure 3B, @bellTimeRequirementPollination2010], we feel that it is reasonable to assume 100% viability of the new, plump seeds.    

The calculation presented here is a rough estimation of how resistance is present in the 2-year versus 4-year rotations under conventional corn weed management. We followed a population of 10000 viable seeds in the 0 -  2 cm soil stratum and 0 seeds in the 2 - 20 cm soil stratum for four years. Even though resistance mutation occur at as low as 10$^{-10}$ to 10$^{-6}$ rate for a single, nuclear completely dominant or recessive gene [@jasieniukEvolutionGeneticsHerbicide1996], we assumed that *at model year 0* 5% of the exposed waterhemp populations at our experiment site were resistant to the applied herbicide active ingredients to match with the survival rates observed at the experiment site in 2018. Let R, T, and S denote the waterhemp seeds that its seedling are resistant, tolerant, and susceptible to the applied herbicide active ingredients. The relative fitness to herbicide of the exposed plants are R > T > S [Chapter 8 of @cousensDynamicsWeedPopulations1995]. The following assumptions are made:    

- In the corn and soybean phases, all the mature plants are R; in the oat and alfalfa phases (occurred in the 4-year rotation only), R, T, and S plants had the same survival rate.    

- In the corn and soybean phases, 50% of the new seeds of plant cohorts 1 through 3 plants  are R and 50% are S and T; but new seeds of cohorts 4 through 6 are 5% R, and 95% T and S combined; in the oat and alfalfa phases (occurred in the 4-year rotation only), all the new seeds in all cohorts are 5% R, and 95% T and S combined.   

- All the seeds were of the same overwinter survival rate, emergence rate, summer survival rate, and vertical movement rate.  






### overwinter survival matrix  


```r
overwinter <- readRDS("../2-Data/Clean/mean-winter-seed-survival-Sosnoskie.RData")
```

### emergence matrix  



### seed survival rate and seedling to maturity success rate 















```r
##### with corn under conventional weed management {-}
#t <- 100
#R_2yr_conv <- list() # blank data frame to save loop output 
#R_2yr_conv[[1]] <- starting_point 

rot_2year_resistance_conv_C(starting_point,
                           poh_C = fall_tillage$C2_conv,
                              ow_C = overwinter$C2_conv,
                              prt_C  = spring_tillage$C2_conv,
                              em_C  = emergence$C2_conv,
                              sv_C = summer_survival$C2_conv,
                              seed_C = fecundity_scenario1$C2_conv,
                            #soybean dynamics   
                              poh_S = fall_tillage$S2_conv,
                              ow_S = overwinter$S2_conv,
                              prt_S  = spring_tillage$S2_conv,
                              em_S  = emergence$S2_conv,
                              sv_S = summer_survival$S2_conv,
                              seed_S = fecundity_scenario1$S2_conv)
```

```
## [1] 0.4987380 0.4675477
```



```r
rot_4year_resistance_conv_C(starting_point,
                           poh_C = fall_tillage$C2_conv,
                              ow_C = overwinter$C2_conv,
                              prt_C  = spring_tillage$C2_conv,
                              em_C  = emergence$C2_conv,
                              sv_C = summer_survival$C2_conv,
                              seed_C = fecundity_scenario1$C2_conv,
                            #soybean dynamics   
                              poh_S = fall_tillage$S2_conv,
                              ow_S = overwinter$S2_conv,
                              prt_S  = spring_tillage$S2_conv,
                              em_S  = emergence$S2_conv,
                              sv_S = summer_survival$S2_conv,
                              seed_S = fecundity_scenario1$S2_conv,
                             #oat dynamics   
                              poh_O = fall_tillage$O4_conv,
                              ow_O = overwinter$O4_conv,
                              prt_O  = spring_tillage$O4_conv,
                              em_O  = emergence$O4_conv,
                              sv_O = summer_survival$O4_conv,
                              seed_O = fecundity_scenario1$O4_conv,
                              #alfalfa dynamics   
                          poh_A = fall_tillage$A4_conv,
                          ow_A = overwinter$A4_conv,
                          prt_A  = spring_tillage$A4_conv,
                          em_A  = emergence$A4_conv,
                          sv_A = summer_survival$A4_conv,
                          seed_A = fecundity_scenario1$A4_conv)
```

```
## [1] 0.2271261 0.1012477
```
 
