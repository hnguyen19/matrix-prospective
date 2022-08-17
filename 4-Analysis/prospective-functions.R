######### Population trajectories #############

### 2-year rotation 

prospective_2year <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                       prt_S, em_S, sv_S, seed_S, poh_S, ow_S){
  
  
  # corn phase dynamics   
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*% vec 
  
  # soybean phase dynamics
  
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S  %*% after_corn 
  
  after_soy
}


### 3-year rotation  

prospective_3year <- function(vec, prt_C, em_C, sv_C, seed_C, poh_C, ow_C, 
                                  prt_S, em_S, sv_S, seed_S, poh_S, ow_S,
                                  prt_O, em_O, sv_O, seed_O, poh_O, ow_O){

  # corn phase dynamics  
  after_corn <- ow_C %*%  poh_C %*% seed_C %*% sv_C %*% em_C %*% prt_C %*%  vec 
  # soybean phase dynamics
  after_soy <- ow_S %*%  poh_S %*% seed_S %*% sv_S %*% em_S %*% prt_S %*%  after_corn
  # oat phase dynamics
  after_oat <-   ow_O %*%  poh_O %*% seed_O %*% sv_O %*% em_O %*% prt_O %*% after_soy 
  
  after_oat
}



### 4-year rotation

prospective_4year <- function(vec, prt_C, em_C, sv_C,  seed_C, poh_C, ow_C,  
         prt_S, em_S, sv_S, seed_S , poh_S, ow_S, 
         prt_O, em_O, sv_O, seed_O, poh_O, ow_O, 
         prt_A, em_A, sv_A, seed_A,poh_A, ow_A){
  

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