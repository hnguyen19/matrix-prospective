### Variance of the lower-level parameters {-}
The variance-covariance matrix of each matrix $B_h$, $V_h$, was used to calculate the variance of $\lambda$. $V_h$ dimension depended on the dimension of the non-zero block in the relevant $B_h$ matrix. All the $V_h$ matrices, but $V_\tau$ and  $V_\gamma$, are diagonal. Variance of zero is assumed to be zero. 

#### Pre-planting and post-harvest tillage induced seed movement {-}

The variance-covariance matrix of matrices $B_{t(s)}$ and $B_{t(f)}$ are 2 x 2 because these periods involved seed dynamics only. $v_{t_{i,j}} = \frac{t_i (1-\tau_i)}{n_.j}$ and $\rho_{t_{i,j}} = -\frac{t_i t_j}{n_.j}$, where $n_.j$ is the column summation of all the seeds in the relevant $B_t$ matrix.  

$$
  V_{t(s)} = \left[\begin{array}
                   {rr} 
                   v_{t_{11,s}} & \rho_{t_{21,s}} \\
                   \rho_{t_{12,s}} & v_{t_{22,s}} \\
                   \end{array}\right]
$$  
  
  and    

$$
  V_{t(f)} = \left[\begin{array}
                   {rr} 
                   v_{t_{11,f}} & \rho_{t_{21,f}} \\
                   \rho_{t_{12,f}} & v_{t_{22,f}} \\
                   \end{array}\right]
$$   
  
  
  #### Seedling emergence {-}
  
  The diagonals of $V_g$ were filled with $\frac{pi(1-p_i)}{n}$ and other elements, where $i \neq j$, were filled with $\frac{-p_ip_j}{n}$. $p_k$ is the proportion of seedling emerge at cohort k. The off-diagonal of the $M_s$ and the second column of the $M_{s,p}$ are zeros because we assumed no emergence from the 2-20 cm soil stratum and that the seedling emergence from the 0-2 cm soil stratum is independent of the 2-20 stratum soil seedbank density, and $n$ is the number of seeds in the 0-2 cm soil stratum. 

$$
  V_g=\left[\begin{array}
            {rr|rrrrr} 
            \frac{(1-\sum_{k=1}^6 g_k)  \sum_{k=1}^6 g_k}{n} & 0 & 0 & ... & ... & ... & ... & 0\\  
            0 & 0 & 0 & ... & ... & ... & ... & 0\\  
            \hline             
            -\frac{d g_3}{n} & 0 & \frac{g_1(1-g_1)}{n} & ... & ... & ... & ... & 0\\  
            ... & ... & -\frac{g_1g_2}{n} & \frac{g_2(1-g_2)}{n} & ... & ... & ... & ...\\  
            -\frac{d g_6} {n} & 0 & 0 & ... & ... & ... & -\frac{g_5g_6}{n} & \frac{g_6(1-g_6)}{n}\\  
            \end{array}\right]
$$  
  
  #### Summer seedling and plant survival {-}
  
  $v_{s_{s_1}}$ and $v_{s_{s_2}}$ elements were visually estimated from Figures 1 and 3, in @sosnoskieGlyphosateResistanceDoes2013 because the raw data was not available. All other elements are 0 because we assumed seed survival in different strata and plant survival in different cohorts are independent of one another.  

$$
  \mathbf{V_s} = \left[\begin{array}
                       {rr|rrrrr} 
                       v_{s_{s_1}}  & 0 & 0 & ... & ... & ... & ... & 0\\
                       0 & v_{s_{s_2}}  & 0 & ... & ... & ... & ... & 0\\
                       \hline             
                       0 & 0 & s_{p_1} & ... & ... & ...&...  & 0\\
                       ... & ... & 0 & s_{p_2} & ... & ... & ... & 0\\ 
                       ... & ... & ... & ... & ... & ... & ... & 0\\ 
                       0 & 0 & 0 & ... & ... & ...& 0 & s_{p_6}\\
                       \end{array}\right]
$$ 
  
  #### Plant fecundity {-}
  
  The variance-covariance matrix of $B_f$, $V_f$, is 6 x 6 because this period involved plant dynamics only. $v_{f_k} = \frac{\sum_{m=1}^z (m_{z} - \bar{m_k})} {n_k - 1}$, where $f_z$ is the fecundity of plant z in cohort k, $\bar{f_k}$ is the mean fecundity of the cohort k, and $n_k$ is the number of plants in cohort k. Variance of a cohort that had only one sample was assigned zero. Fecundity of plants in different cohorts were assumed independent, so $\rho(f_i,f_j) = 0$.  

$$
  \mathbf{V_f} = \left[\begin{array}
                       {rrrrr} 
                       v_{f_1} & 0 & ... & ... & ... & 0 & \\
                       ... & ...& ... & ... &...&...\\
                       ... & ... & ... & ... & v_{f_5} & ... \\ 
                       0 & ... & ... & ... & 0 & v_{f_6} \\
                       \end{array}\right]
$$   
  

#### Overwinter survival {-}

Similar to $V_s$, the variance-covariance matrix of $B_o$, $V_o$, is 2x2 because this stage involved seed dynamics only. The $v_o_{11}$ and $v_{o_{22}$ elements were visually estimated from Figures 1 and 3, in @sosnoskieGlyphosateResistanceDoes2013 because the raw data was not available. Survival rates of seeds in different strata were assumed independent, so $\rho(f_i,f_j) = 0$
  
  $$
  V_o = \left[\begin{array}
              {rr} 
              v_{o_{11}} & 0 \\
              0  & v_{o_{22}} \\
              \end{array}\right]
  $$   




    