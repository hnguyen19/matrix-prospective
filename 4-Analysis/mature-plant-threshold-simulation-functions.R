
################################################ 2-year rotation ################################################

###### Lambda calculation #######

rot_2year_conv_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                           prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
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

rot_2year_low_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                          prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
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

###### Mature plant density output #######

rot_2year_conv_plant_density <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                               prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
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
  
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8])
  
  names(l) <- c("C2", "S2")
  
  l
}


rot_2year_low_plant_density <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                              prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
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
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8])
  
  names(l) <- c("C2", "S2")
  
  l
}

###### Seed production, fixed #######
### from fecundity18 <- readRDS("../2-Data/Clean/mean-fecundity-18-cohort.RData")
rot_2year_conv_seed_production_per_cap_fixed <- function(seed_C, seed_S){
  

  seed_production_per_cap_corn_fixed <- seed_C[1,3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1,3:8]  
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed, seed_production_per_cap_soy_fixed)
  
  names(l) <- c("C2", "S2")
  
  l 
  
}

rot_2year_low_seed_production_per_cap_fixed <- function(seed_C, seed_S){
  

  seed_production_per_cap_corn_fixed <- seed_C[1,3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1,3:8]  
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed, seed_production_per_cap_soy_fixed)
  
  names(l) <- c("C2", "S2")
  
  l 
}

################################################ 3-year rotation ################################################
###### Lambda calculation #######
rot_3year_conv_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                           prt_S, em_S, sv_S, seed_S, poh_S, ow_S,
                           prt_O, em_O, sv_O, seed_O, poh_O, ow_O){
  
  
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

rot_3year_low_lambda <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                          prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                          prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
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

###### Mature plant density output #######
### given manipulated per-capita seed production that make annualized lambda = 1
rot_3year_conv_plant_density <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                                               prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                                               prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
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
  
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  # oat phase dynamics 
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  pl_dens_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy 
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8])
  
  names(l) <- c("C3", "S3", "O3")
  
  l
}

rot_3year_low_plant_density <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                                              prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                                              prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
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
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  # oat phase dynamics 
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  pl_dens_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy 
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8])
  
  names(l) <- c("C3", "S3", "O3")
  
  l
}


###### Seed production, fixed #######
### from fecundity18 <- readRDS("../2-Data/Clean/mean-fecundity-18-cohort.RData")
rot_3year_conv_seed_production_per_cap_fixed <- function(seed_C, seed_S, seed_O){
  

  
  seed_production_per_cap_corn_fixed <- seed_C[1, 3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1, 3:8]  
  
  seed_production_per_cap_oat_fixed <- seed_O[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed,
            seed_production_per_cap_soy_fixed,
            seed_production_per_cap_oat_fixed)
  
  names(l) <- c("C3", "S3", "O3")
  
  l 
  
}

rot_3year_low_seed_production_per_cap_fixed <- function(seed_C, seed_S, seed_O){
  

  
  seed_production_per_cap_corn_fixed <- seed_C[1,3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1,3:8]  
  
  seed_production_per_cap_oat_fixed <- seed_O[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed,
            seed_production_per_cap_soy_fixed,
            seed_production_per_cap_oat_fixed)
  
  names(l) <- c("C3", "S3", "O3")
  
  l 
  
}

################################################ 4-year rotation ################################################
###### Lambda calculation #######

### conventional weed management
rot_4year_conv_lambda <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                           prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                           prt_O, em_O, sv_O, seed_O, poh_O, ow_O, 
                           prt_A, em_A, sv_A, seed_A,poh_A, ow_A){
  
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
rot_4year_low_lambda <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                          prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                          prt_O, em_O, sv_O, seed_O, poh_O, ow_O, 
                          prt_A, em_A, sv_A, seed_A,poh_A, ow_A){
  
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



###### Mature plant density output #######

rot_4year_conv_plant <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                                               poh_S, ow_S, prt_S, em_S, sv_S, seed_S,
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
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  # oat phase dynamics 
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  pl_dens_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy 
  
  # alfalfa phase dynamics
  after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  
  pl_dens_alfalfa <-  sv_A %*% em_A %*% prt_A %*% after_oat 
  
  # Collect phase-end mature plant density    
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8])
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l
}

rot_4year_low_plant_density <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                                              poh_S, ow_S, prt_S, em_S, sv_S, seed_S,
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
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  # oat phase dynamics 
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  pl_dens_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy 
  
  # alfalfa phase dynamics
  after_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  
  pl_dens_alfalfa <-  sv_A %*% em_A %*% prt_A %*% after_oat 
  
  # Collect phase-end mature plant density    
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8], pl_dens_oat[3:8], pl_dens_alfalfa[3:8])
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l
}

###### Seed production, fixed #######
### from fecundity18 <- readRDS("../2-Data/Clean/mean-fecundity-18-cohort.RData")
rot_4year_conv_seed_production_per_cap_fixed <- function(seed_C, seed_S, seed_O, seed_A){

  
  seed_production_per_cap_corn_fixed <- seed_C[1, 3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1, 3:8]  
  
  seed_production_per_cap_oat_fixed <- seed_O[1, 3:8]
  
  seed_production_per_cap_alfalfa_fixed <- seed_A[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed, seed_production_per_cap_soy_fixed,
            seed_production_per_cap_oat_fixed, seed_production_per_cap_alfalfa_fixed)
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l 
  
}

rot_4year_low_seed_production_per_cap_fixed <- function(seed_C, seed_S, seed_O, seed_A){
  

  
  seed_production_per_cap_corn_fixed <- seed_C[1,3:8] 
  
  seed_production_per_cap_soy_fixed <- seed_S[1,3:8]  
  
  seed_production_per_cap_oat_fixed <- seed_O[1, 3:8]
  
  seed_production_per_cap_alfalfa_fixed <- seed_A[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_per_cap_corn_fixed, seed_production_per_cap_soy_fixed,
            seed_production_per_cap_oat_fixed, seed_production_per_cap_alfalfa_fixed)
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l 
  
}
