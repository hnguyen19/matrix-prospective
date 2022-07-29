```{r recruitment-matrix-new-unadjusted}

## Old: Emergence proportion of the twice-shuffled seed columns by element-wise division
# mean_emergence_prop <- purrr::map2(emerge_20, mean_after_spring_tillage_pop_scenario1_dummy, `/`)

## New: Emergence proportion of the twice-shuffled seed columns in a df merged with the mean_after_spring_tillage_pop_scenario1_df piece
## Theoretically, the covariance of top and bottom seeds props are non-zeros, but this particular matrix only concerns the relationships of the top stratum and 6 seedling cohorts. 
female_emerge_prop_20 <- female_emerge_20_mean_wide %>% 
  left_join(mean_after_spring_tillage_pop_scenario1_top_stratum_df, by = "matrix_id") %>%
  mutate(cohort1_mean_prop = `1`/top_stratum_female_density,
         cohort2_mean_prop = `2`/top_stratum_female_density,
         cohort3_mean_prop = `3`/top_stratum_female_density,
         cohort4_mean_prop = `4`/top_stratum_female_density,
         cohort5_mean_prop = `5`/top_stratum_female_density,
         cohort6_mean_prop = `6`/top_stratum_female_density,
         top_mean_remain_prop = 1-(cohort1_mean_prop + cohort2_mean_prop + cohort3_mean_prop +
                                     cohort4_mean_prop + cohort5_mean_prop + cohort6_mean_prop)) %>%
  mutate(top_var_remain_prop = top_mean_remain_prop*(1-top_mean_remain_prop)*top_stratum_density,
         bottom_var = 0,
         cohort1_var_prop = cohort1_mean_prop*(1 - cohort1_mean_prop)*top_stratum_female_density,
         cohort2_var_prop = cohort2_mean_prop*(1 - cohort2_mean_prop)*top_stratum_female_density,
         cohort3_var_prop = cohort3_mean_prop*(1 - cohort3_mean_prop)*top_stratum_female_density,
         cohort4_var_prop = cohort4_mean_prop*(1 - cohort4_mean_prop)*top_stratum_female_density,
         cohort5_var_prop = cohort5_mean_prop*(1 - cohort5_mean_prop)*top_stratum_female_density,
         cohort6_var_prop = cohort6_mean_prop*(1 - cohort6_mean_prop)*top_stratum_female_density) %>%
  mutate(top_cohort1_covar = -top_mean_remain_prop*cohort1_mean_prop/top_stratum_female_density,
         top_cohort2_covar = -top_mean_remain_prop*cohort2_mean_prop/top_stratum_female_density,
         top_cohort3_covar = -top_mean_remain_prop*cohort3_mean_prop/top_stratum_female_density,
         top_cohort4_covar = -top_mean_remain_prop*cohort4_mean_prop/top_stratum_female_density,
         top_cohort5_covar = -top_mean_remain_prop*cohort5_mean_prop/top_stratum_female_density,
         top_cohort6_covar = -top_mean_remain_prop*cohort6_mean_prop/top_stratum_female_density,
         cohort1_2_covar = -cohort1_mean_prop*cohort2_mean_prop/top_stratum_female_density,
         cohort1_3_covar = -cohort1_mean_prop*cohort3_mean_prop/top_stratum_female_density,
         cohort1_4_covar = -cohort1_mean_prop*cohort4_mean_prop/top_stratum_female_density,
         cohort1_5_covar = -cohort1_mean_prop*cohort5_mean_prop/top_stratum_female_density,
         cohort1_6_covar = -cohort1_mean_prop*cohort6_mean_prop/top_stratum_female_density,
         cohort2_3_covar = -cohort2_mean_prop*cohort3_mean_prop/top_stratum_female_density,
         cohort2_4_covar = -cohort2_mean_prop*cohort4_mean_prop/top_stratum_female_density,
         cohort2_5_covar = -cohort2_mean_prop*cohort5_mean_prop/top_stratum_female_density,
         cohort2_6_covar = -cohort2_mean_prop*cohort6_mean_prop/top_stratum_female_density,
         cohort3_4_covar = -cohort3_mean_prop*cohort4_mean_prop/top_stratum_female_density,
         cohort3_5_covar = -cohort3_mean_prop*cohort5_mean_prop/top_stratum_female_density,
         cohort3_6_covar = -cohort3_mean_prop*cohort6_mean_prop/top_stratum_female_density,
         cohort4_5_covar = -cohort4_mean_prop*cohort5_mean_prop/top_stratum_female_density,
         cohort4_6_covar = -cohort4_mean_prop*cohort6_mean_prop/top_stratum_female_density,
         cohort5_6_covar = -cohort5_mean_prop*cohort6_mean_prop/top_stratum_female_density)

### Mean matrix list 
mean_female_emerge_prop_20 <- female_emerge_prop_20 %>%
  select(matrix_id, cohort1_mean_prop:cohort6_mean_prop) %>%
  pivot_longer(!matrix_id, names_to = "Cohort", values_to = "mean_emerge_prop")

# https://stackoverflow.com/questions/15897236/extract-the-first-or-last-n-characters-of-a-string
#str_right <- function(string, n) {
#  substr(string, nchar(string) - (n - 1), nchar(string))
#}
# https://stackoverflow.com/questions/23413331/how-to-remove-last-n-characters-from-every-element-in-the-r-vector
#emergence_prop_df <- emerge_prop_20 %>%
#  map(~.x[1:6, ])  %>% 
#  unlist(use.names = TRUE) %>%
#  as.data.frame() %>%
#  rownames_to_column("matrix_id") %>% 
#  rename(mean_emerge_prop = ".") %>%
#  mutate(Cohort = str_right(matrix_id, 1),
#         new_matrix_id = substr(matrix_id, 1, nchar(matrix_id)-1))


#mean_emergence_prop_matrix_list <- lapply(split(mean_female_emerge_prop_20, mean_female_emerge_prop_20$matrix_id),
#       function(x) rbind(cbind(matrix(c(1-sum(x$mean_female_emerge_prop), 0,0,1), nrow = 2, byrow = TRUE), matrix(0,nrow = 2, ncol = 6)),
#       cbind(matrix(x$mean_female_emerge_prop, nrow = 6, ncol = 1),matrix(0,nrow =6, ncol=7))))

### write the emergence proportion matrix
# saveRDS(mean_emergence_prop_matrix_list , file="../2-Data/Clean/mean-emergence-prop.RData")

### Var-covar matrix list

var_emerge_prop_20 <- emerge_prop_20 %>%
  select(matrix_id, top_var_remain_prop:cohort6_var_prop)  %>%
  pivot_longer(!matrix_id, names_to = "Cohort", values_to = "var_emerge_prop")

var_emerge_prop_matrix_list <- lapply(split(var_emerge_prop_20, var_emerge_prop_20$matrix_id),
                                      function(x) diag(x$var_emerge_prop,8))

covar_emerge_prop_20 <- emerge_prop_20 %>%
  select(matrix_id, top_cohort1_covar:cohort5_6_covar)

covar_emerge_prop_matrix_list <- lapply(split(covar_emerge_prop_20, covar_emerge_prop_20$matrix_id),
                                        function(x) rbind(c(0, 0, x[,2:7]),
                                                          rep(0,8),
                                                          c(x[,2], 0, 0, x[,8:12]),
                                                          c(x[,3], 0, x[,8], 0, x[,13:16]),
                                                          c(x[,4], 0, x[,9], x[,13], 0, x[, 17:19]),
                                                          c(x[,5], 0, x[,10], x[,14], x[,17], 0, x[, 20:21]),
                                                          c(x[,6], 0, x[,11], x[,15], x[,18], x[,20], 0, x[,22]),
                                                          c(x[,7], 0, x[,12], x[,16], x[,19], x[,21:22], 0) ))

#convert the list of double to a list of vector

covar_emerge_prop_matrix_unlist <- lapply(covar_emerge_prop_matrix_list, unlist)

#convert the list of vector to a list of matrix
# https://stackoverflow.com/questions/49261628/using-purrr-to-convert-list-of-vectors-to-list-of-matrices
covar_emerge_prop_matrix_list_m <- covar_emerge_prop_matrix_unlist %>%
  map(~ matrix(., 8,8, byrow = FALSE)) 

## combine the var and covar into a matrix 

var_covar_emerge_prop_20_list <-  purrr::map2(var_emerge_prop_matrix_list,
                                              covar_emerge_prop_matrix_list_m, `+`)

### write the emergence proportion var-covar matrix
# saveRDS(var_covar_emerge_prop_20_list , file="../2-Data/Clean/var-emergence-prop-scenario1.RData")
```
