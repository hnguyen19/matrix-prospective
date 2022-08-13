
################################################ 2-year rotation ################################################

###### Lambda calculation #######

rot_2year_conv_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                           prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  seed_C[1,3] <- rlnorm(1, 5.55, 0.48) #257.03 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.34, 0.5) # 208.18 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.34, 0.5) 
  
  
  seed_S[1,3] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,4] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,5] <- rlnorm(1, 5.75, 0.46) #316.83
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  
  # soybean phase dynamics
  
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  after_soy
}

rot_2year_low_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                          prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  seed_C[1,3] <- rlnorm(1, 3.85, 0.7) # 46.55 seeds/plant
  seed_C[1,4] <- rlnorm(1, 3.44, 0.76) # 30.84 seeds/plant
  seed_C[1,5] <- rlnorm(1, 3.85, 0.7)
  
  seed_S[1,3] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,4] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,5] <- rlnorm(1, 4.22, 0.65) #67.56 seeds/plant
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  # soybean phase dynamics
  
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  after_soy
}

###### Mature plant density output #######
### given manipulated per-capita seed production that make annualized lambda = 1
rot_2year_conv_plant_density_fixed <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                               prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  seed_C[1,3] <- rlnorm(1, 5.55, 0.48) #257.03 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.34, 0.5) # 208.18 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.34, 0.5) 
  
  
  seed_S[1,3] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,4] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,5] <- rlnorm(1, 5.75, 0.46) #316.83 seeds/plant
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8])
  
  names(l) <- c("C2", "S2")
  
  l
}


rot_2year_low_plant_density_fixed <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                              prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  seed_C[1,3] <- rlnorm(1, 3.85, 0.7) # 46.55 seeds/plant
  seed_C[1,4] <- rlnorm(1, 3.44, 0.76) # 30.84 seeds/plant
  seed_C[1,5] <- rlnorm(1, 3.85, 0.7)
  
  seed_S[1,3] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,4] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,5] <- rlnorm(1, 4.22, 0.65) #67.56 seeds/plant
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  pl_dens_corn <-  sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  
  pl_dens_soy  <-  sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  
  l <- list(pl_dens_corn[3:8], pl_dens_soy[3:8])
  
  names(l) <- c("C2", "S2")
  
  l
}

###### Seed production, manipulated #######
rot_2year_conv_seed_production_per_cap <- function(seed_C, seed_S){
  
  
  seed_C[1,3] <- rlnorm(1, 5.55, 0.48) #257.03 seeds/plant
  seed_C[1,4] <- rlnorm(1, 5.34, 0.5) # 208.18 seeds/plant
  seed_C[1,5] <- rlnorm(1, 5.34, 0.5) 
  
  
  seed_S[1,3] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,4] <- rlnorm(1, 5.55, 0.48) 
  seed_S[1,5] <- rlnorm(1, 5.75, 0.46) #316.83 seeds/plant
  
  
  seed_production_count_corn <- seed_C[1,3:8] 
  
  seed_production_count_soy <- seed_S[1,3:8]  
  
  # seed at harvest
  
  l <- list(seed_production_count_corn, seed_production_count_soy)
  
  names(l) <- c("C2", "S2")
  
  l 
  
}

rot_2year_low_seed_production_per_cap <- function(seed_C, seed_S){
  
  
  seed_C[1,3] <- rlnorm(1, 3.85, 0.7) # 46.55 seeds/plant
  seed_C[1,4] <- rlnorm(1, 3.44, 0.76) # 30.84 seeds/plant
  seed_C[1,5] <- rlnorm(1, 3.85, 0.7)
  
  seed_S[1,3] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,4] <- rlnorm(1, 3.85, 0.7)
  seed_S[1,5] <- rlnorm(1, 4.22, 0.65) #67.56 seeds/plant
  
  
  seed_production_count_corn <- seed_C[1,3:8] 
  
  seed_production_count_soy <- seed_S[1,3:8]  
  
  # seed at harvest
  
  l <- list(seed_production_count_corn, seed_production_count_soy)
  
  names(l) <- c("C2", "S2")
  
  l 
}

################################################ 3-year rotation ################################################
###### Lambda calculation #######
rot_3year_conv_lambda <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                           prt_S, em_S, sv_S, seed_S, poh_S, ow_S,
                           prt_O, em_O, sv_O, seed_O, poh_O, ow_O){
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,8] <- rlnorm(1, 2.66, 0.89)
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 2.66, 0.89)
   seed_S[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,8] <- rlnorm(1, 2.66, 0.89)
  
  
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
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 5.05, 0.53)
  
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 6.94, 0.43)
  
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
rot_3year_conv_plant_density_fixed <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                                               prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                                               prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,8] <- rlnorm(1, 2.66, 0.89)
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,8] <- rlnorm(1, 2.66, 0.89)
  
  
  
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

rot_3year_low_plant_density_fixed <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
                                              prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
                                              prt_O, em_O, sv_O, seed_O, poh_O, ow_O ){
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 5.05, 0.53)
  
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 6.94, 0.43)
  
  
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


###### Seed production, manipulated #######
rot_3year_conv_seed_production_per_cap <- function(seed_C, seed_S, seed_O){
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_C[1,8] <- rlnorm(1, 2.66, 0.89)
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,6] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,7] <- rlnorm(1, 2.66, 0.89)
    seed_S[1,8] <- rlnorm(1, 2.66, 0.89)
  
  
  seed_production_count_corn <- seed_C[1, 3:8] 
  
  seed_production_count_soy <- seed_S[1, 3:8]  
  
  seed_production_count_oat <- seed_O[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_count_corn,
            seed_production_count_soy,
            seed_production_count_oat)
  
  names(l) <- c("C3", "S3", "O3")
  
  l 
  
}

rot_3year_low_seed_production_per_cap <- function(seed_C, seed_S, seed_O){
  
  
  seed_C[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_C[1,5] <- rlnorm(1, 5.05, 0.53)
  
  
  seed_S[1,3] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,4] <- rlnorm(1, 2.66, 0.89)
  seed_S[1,5] <- rlnorm(1, 6.94, 0.43)
  
  
  seed_production_count_corn <- seed_C[1,3:8] 
  
  seed_production_count_soy <- seed_S[1,3:8]  
  
  seed_production_count_oat <- seed_O[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_count_corn,
            seed_production_count_soy,
            seed_production_count_oat)
  
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
  
  seed_C[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,5] <- rlnorm(1,  2.66, 0.89)
  #fecundity was much lower after cohort 3, so focus on suppressing plant size in soybean
  
  
  seed_S[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,6] <- rlnorm(1,  7.34, 0.44)
  
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
### given manipulated per-capita seed production that make annualized lambda = 1
rot_4year_conv_plant_density_fixed <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                                               poh_S, ow_S, prt_S, em_S, sv_S, seed_S,
                                               poh_O, ow_O, prt_O, em_O, sv_O, seed_O,
                                               poh_A, ow_A, prt_A, em_A, sv_A, seed_A){
  
  
  seed_C[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,5] <- rlnorm(1,  2.66, 0.89)
  #fecundity was much lower after cohort 3, so focus on suppressing plant size in soybean
  
  
  seed_S[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,6] <- rlnorm(1,  7.34, 0.44)
  
  
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

rot_4year_low_plant_density_fixed <- function(vec, poh_C, ow_C, prt_C, em_C, sv_C, seed_C,
                                              poh_S, ow_S, prt_S, em_S, sv_S, seed_S,
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

###### Seed production, manipulated #######
rot_4year_conv_seed_production_per_cap <- function(seed_C, seed_S, seed_O, seed_A){
  
  
  
  seed_C[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_C[1,5] <- rlnorm(1,  2.66, 0.89)
  #fecundity was much lower after cohort 3, so focus on suppressing plant size in soybean
  
  
  seed_S[1,3] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,4] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,5] <- rlnorm(1,  2.66, 0.89)
  seed_S[1,6] <- rlnorm(1,  7.34, 0.44)
  
  
  seed_production_count_corn <- seed_C[1, 3:8] 
  
  seed_production_count_soy <- seed_S[1, 3:8]  
  
  seed_production_count_oat <- seed_O[1, 3:8]
  
  seed_production_count_alfalfa <- seed_A[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_count_corn, seed_production_count_soy,
            seed_production_count_oat, seed_production_count_alfalfa)
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l 
  
}

rot_4year_low_seed_production_per_cap <- function(seed_C, seed_S, seed_O, seed_A){
  
  
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
  
  
  seed_production_count_corn <- seed_C[1,3:8] 
  
  seed_production_count_soy <- seed_S[1,3:8]  
  
  seed_production_count_oat <- seed_O[1, 3:8]
  
  seed_production_count_alfalfa <- seed_A[1, 3:8]
  
  # seed at harvest
  
  l <- list(seed_production_count_corn, seed_production_count_soy,
            seed_production_count_oat, seed_production_count_alfalfa)
  
  names(l) <- c("C4", "S4", "O4", "A4")
  
  l 
  
}
