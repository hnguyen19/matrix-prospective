
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
  sv_C[4,4] <- .001
  sv_C[5,5] <- .001 # 99.8% efficacy
 # sv_C[6,6] <- .001
#  sv_C[7,7] <- .001
#  sv_C[8,8] <- .001

  sv_S[3,3] <- .001
  sv_S[4,4] <-  .001
  sv_S[5,5] <-  .001
#  sv_S[6,6] <- .001
#  sv_S[7,7] <- .001
#  sv_S[8,8] <- .001
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  # soybean phase dynamics
  
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  after_soy
}

###### Manipulated survival rate and resulting mature plant density #######
rot_2year_conv_manipulated_outputs <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
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
  
  manipulated_mature_plant_density_in_corn <- sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn  <-  seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  manipulated_mature_plant_density_in_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # corn phase dynamics   
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8], manipulated_seed_production_in_soy)
  
  names(l) <- c("C2_manipulated_survive", "C2_manipulated_mature_plant_density", "C2_manipulated_seed_production",
                "S2_manipulated_survive", "S2_manipulated_mature_plant_density", "S2_manipulated_seed_production")
  
  l

}

rot_2year_low_manipulated_outputs <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                   prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  sv_C[3,3] <- .001 #99.9 % efficacy
  sv_C[4,4] <- .001
  sv_C[5,5] <- .001 # 99.8% efficacy
  # sv_C[6,6] <- .001
  #  sv_C[7,7] <- .001
  #  sv_C[8,8] <- .001
  
  sv_S[3,3] <- .001
  sv_S[4,4] <-  .001
  sv_S[5,5] <-  .001
  #  sv_S[6,6] <- .001
  #  sv_S[7,7] <- .001
  #  sv_S[8,8] <- .001
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_mature_plant_density_in_corn <- sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn  <-  seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  manipulated_mature_plant_density_in_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # corn phase dynamics   
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8],manipulated_seed_production_in_soy)
  
  names(l) <- c("C2_manipulated_survive", "C2_manipulated_mature_plant_density", "C2_manipulated_seed_production",
                "S2_manipulated_survive", "S2_manipulated_mature_plant_density", "S2_manipulated_seed_production")
  
  l
  
}

###### Original survival, mature plant density, and seed production  #######
# Only 1 function because no changes were applied

rot_2year_original_outputs <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                               prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  

  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  original_mature_plant_density_in_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
 original_seed_production_in_corn <- seed_C[1,3:8] * original_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  
  original_mature_plant_density_in_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  original_seed_production_in_soy <- seed_S[1,3:8] * original_mature_plant_density_in_soy[3:8] 
  
  
  l <- list(diag(sv_C)[3:8], original_mature_plant_density_in_corn[3:8], original_seed_production_in_corn,
            diag(sv_S)[3:8], original_mature_plant_density_in_soy[3:8],original_seed_production_in_soy)
  
  names(l) <- c("C2_original_survive", "C2_original_mature_plant_density", "C2_original_seed_production",
                "S2_original_survive", "S2_original_mature_plant_density", "S2_original_seed_production")
  
  l
}



###### Seed production, resulted from manipulated #######
### from fecundity18 <- readRDS("../2-Data/Clean/mean-fecundity-18-cohort.RData")
# only one function because no chain multiplication is performed
# marked original because no manipulated applied to sv_X



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

###### Manipulated survival rate and resulting mature plant density #######

rot_3year_conv_manipulated_outputs <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
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
  
  ## Extra control efficacy in oat is now needed  
  sv_O[5,5] <- .01
  sv_O[6,6] <- .01
  sv_O[7,7] <- .01
  sv_O[8,8] <- .01
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_mature_plant_density_in_corn <- sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn  <-  seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn
  
  manipulated_mature_plant_density_in_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics 
  manipulated_mature_plant_density_in_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  manipulated_seed_production_in_oat  <-  sv_O[1,3:8] * manipulated_mature_plant_density_in_oat[3:8]  
  
  # corn phase dynamics   
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8], manipulated_seed_production_in_soy,
            diag(sv_O)[3:8], manipulated_mature_plant_density_in_oat[3:8], manipulated_seed_production_in_oat)
  
  names(l) <- c("C3_manipulated_survive", "C3_manipulated_mature_plant_density", "C3_manipulated_seed_production", 
                "S3_manipulated_survive", "S3_manipulated_mature_plant_density", "S3_manipulated_seed_production",
                "O3_manipulated_survive", "O3_manipulated_mature_plant_density", "O3_manipulated_seed_production")
  
  l
}

rot_3year_low_manipulated_outputs  <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                                              prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                                              prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  sv_C[3,3] <- .0001
  sv_C[4,4] <- .003
  sv_C[5,5] <- .003

  sv_S[3,3] <- .0001
  sv_S[4,4] <- .003
  sv_S[5,5] <- .003

  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_mature_plant_density_in_corn <- sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn  <-  seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn
  
  manipulated_mature_plant_density_in_soy <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics 
  manipulated_mature_plant_density_in_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  manipulated_seed_production_in_oat  <-  sv_O[1,3:8] * manipulated_mature_plant_density_in_oat[3:8]  
  
  # corn phase dynamics   
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8], manipulated_seed_production_in_soy,
            diag(sv_O)[3:8], manipulated_mature_plant_density_in_oat[3:8], manipulated_seed_production_in_oat)
  
  names(l) <- c("C3_manipulated_survive", "C3_manipulated_mature_plant_density", "C3_manipulated_seed_production", 
                "S3_manipulated_survive", "S3_manipulated_mature_plant_density", "S3_manipulated_seed_production",
                "O3_manipulated_survive", "O3_manipulated_mature_plant_density", "O3_manipulated_seed_production")
  
  l
}

rot_3year_original_outputs <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                       prt_S, em_S, sv_S, seed_S, poh_S, ow_S, 
                                       prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  original_mature_plant_density_in_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  original_seed_production_in_corn <- seed_C[1,3:8] * original_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  original_mature_plant_density_in_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  original_seed_production_in_soy <- seed_S[1,3:8] * original_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics
  
  original_mature_plant_density_in_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy
  
  original_seed_production_in_oat <- seed_O[1,3:8] * original_mature_plant_density_in_oat[3:8] 
  
  
  l <- list(diag(sv_C)[3:8], original_mature_plant_density_in_corn[3:8], original_seed_production_in_corn,
            diag(sv_S)[3:8], original_mature_plant_density_in_soy[3:8], original_seed_production_in_soy,
            diag(sv_O)[3:8], original_mature_plant_density_in_oat[3:8], original_seed_production_in_oat)
  
  names(l) <- c("C3_original_survive", "C3_original_mature_plant_density", "C3_original_seed_production",
                "S3_original_survive", "S3_original_mature_plant_density", "S3_original_seed_production",
                "O3_original_survive", "O3_original_mature_plant_density", "O3_original_seed_production")
  
  l
}


################################################ 4-year rotation ################################################
###### Lambda calculation #######

### conventional weed management
rot_4year_conv_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C,  
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



###### Manipulated outputs: survival, mature density, and seed production  #######

rot_4year_conv_manipulated_outputs  <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
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
  
  manipulated_mature_plant_density_in_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn <- seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_mature_plant_density_in_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  manipulated_mature_plant_density_in_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy
  
  manipulated_seed_production_in_oat <- seed_O[1,3:8] * manipulated_mature_plant_density_in_oat[3:8] 
  
  # alfalfa phase dynamics
  manipulated_mature_plant_density_in_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  
  manipulated_mature_plant_density_in_alfalfa  <-  sv_A %*% em_A %*% prt_A  %*% after_oat
  
  manipulated_seed_production_in_alfalfa <-  seed_A[1,3:8] * manipulated_mature_plant_density_in_alfalfa[3:8] 
  
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8], manipulated_seed_production_in_soy,
            diag(sv_O)[3:8], manipulated_mature_plant_density_in_oat[3:8], manipulated_seed_production_in_oat,
            diag(sv_A)[3:8], manipulated_mature_plant_density_in_alfalfa[3:8], manipulated_seed_production_in_alfalfa)
  
  names(l) <- c("C4_manipulated_survive", "C4_manipulated_mature_plant_density", "C4_manipulated_seed_production",
                "S4_manipulated_survive", "S4_manipulated_mature_plant_density", "S4_manipulated_seed_production",
                "O4_manipulated_survive", "O4_manipulated_mature_plant_density", "O4_manipulated_seed_production",
                "A4_manipulated_survive", "A4_manipulated_mature_plant_density", "A4_manipulated_seed_production")
  
  l
}

rot_4year_low_manipulated_outputs  <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
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
  
  manipulated_mature_plant_density_in_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  manipulated_seed_production_in_corn <- seed_C[1,3:8] * manipulated_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_mature_plant_density_in_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  manipulated_seed_production_in_soy <- seed_S[1,3:8] * manipulated_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  manipulated_mature_plant_density_in_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy
  
  manipulated_seed_production_in_oat <- seed_O[1,3:8] * manipulated_mature_plant_density_in_oat[3:8] 
  
  # alfalfa phase dynamics
  manipulated_mature_plant_density_in_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  
  manipulated_mature_plant_density_in_alfalfa  <-  sv_A %*% em_A %*% prt_A  %*% after_oat
  
  manipulated_seed_production_in_alfalfa <-  seed_A[1,3:8] * manipulated_mature_plant_density_in_alfalfa[3:8] 
  
  l <- list(diag(sv_C)[3:8], manipulated_mature_plant_density_in_corn[3:8], manipulated_seed_production_in_corn,
            diag(sv_S)[3:8], manipulated_mature_plant_density_in_soy[3:8], manipulated_seed_production_in_soy,
            diag(sv_O)[3:8], manipulated_mature_plant_density_in_oat[3:8], manipulated_seed_production_in_oat,
            diag(sv_A)[3:8], manipulated_mature_plant_density_in_alfalfa[3:8], manipulated_seed_production_in_alfalfa)
  
  names(l) <- c("C4_manipulated_survive", "C4_manipulated_mature_plant_density", "C4_manipulated_seed_production",
                "S4_manipulated_survive", "S4_manipulated_mature_plant_density", "S4_manipulated_seed_production",
                "O4_manipulated_survive", "O4_manipulated_mature_plant_density", "O4_manipulated_seed_production",
                "A4_manipulated_survive", "A4_manipulated_mature_plant_density", "A4_manipulated_seed_production")
  
  l
}

####### Original outputs: survival rate, mature plant density, and seed production #######

rot_4year_original_outputs <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                       prt_S, em_S, sv_S, seed_S, poh_S, ow_S, 
                                       prt_O, em_O, sv_O, seed_O, poh_O, ow_O, 
                                       poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  original_mature_plant_density_in_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  original_seed_production_in_corn <- seed_C[1,3:8] * original_mature_plant_density_in_corn[3:8] 
  
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  original_mature_plant_density_in_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  original_seed_production_in_soy <- seed_S[1,3:8] * original_mature_plant_density_in_soy[3:8] 
  
  # oat phase dynamics
  after_oat <-  ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  original_mature_plant_density_in_oat  <-  sv_O %*% em_O %*% prt_O  %*% after_soy
  
  original_seed_production_in_oat <- seed_O[1,3:8] * original_mature_plant_density_in_oat[3:8] 
  
  # alfalfa phase dynamics
  original_mature_plant_density_in_alfalfa <-   ow_A %*%  poh_A %*% seed_A %*% sv_A %*% em_A %*% prt_A %*% after_oat 
  
  original_mature_plant_density_in_alfalfa  <-  sv_A %*% em_A %*% prt_A  %*% after_oat
  
  original_seed_production_in_alfalfa <-  seed_A[1,3:8] * original_mature_plant_density_in_alfalfa[3:8] 
  
  
  l <- list(diag(sv_C)[3:8], original_mature_plant_density_in_corn[3:8], original_seed_production_in_corn,
            diag(sv_S)[3:8], original_mature_plant_density_in_soy[3:8], original_seed_production_in_soy,
            diag(sv_O)[3:8], original_mature_plant_density_in_oat[3:8], original_seed_production_in_oat,
            diag(sv_A)[3:8], original_mature_plant_density_in_alfalfa[3:8], original_seed_production_in_alfalfa)
  
  names(l) <- c("C4_original_survive", "C4_original_mature_plant_density", "C4_original_seed_production",
                "S4_original_survive", "S4_original_mature_plant_density", "S4_original_seed_production",
                "O4_original_survive", "O4_original_mature_plant_density", "O4_original_seed_production",
                "A4_original_survive", "A4_original_mature_plant_density", "A4_original_seed_production")
  
  l
}