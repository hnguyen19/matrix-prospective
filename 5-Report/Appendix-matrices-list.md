---
output:
    bookdown::word_document2:
      toc: false
      fig_caption: yes
      keep_md: yes
bibliography: WH-pop-dynamics.bib
csl: apa-no-ampersand.csl 
---





The structure of all periodic matrices used in the two different scenarios are listed below. All numbers are female-only. Each theoretical matrix for a sub-annual period is followed by the set of matrices used in that sub-annual period. The abbreviate row and column names are:  
- s_t: seed at the top stratum (0 - 2 cm),    
- s_b: seed at the bottom stratum (2 - 20 cm),  
- p_co_1 through p_co_6: plant cohort 1 through 6.  

#### Published literature data {-}

##### Pre-planting tillage induced vertical redistribution of seeds {-}  

The only non-zeroes section of the pre-planting tillage induced vertical redistribution of seeds is $M_s$. $M_s$'s were resized from the raw data of Seed Chaser [@spokasSeedChaserVerticalSoil2007], a simulation program that estimates vertical seed movement after various types of tillage: the proportion of seeds staying at its original soil stratum, $t_{11,s}$ and $t_{22,s}$, or move to another stratum, $t_{12,s}$ and $t_{21,s}$. The original matrices in @spokasSeedChaserVerticalSoil2007 were resized to 2 x 2 by summing over all the elements within each of the four sections, i.e., top left 2 x 2, bottom left 18 x 2, top right 2 x 18, and 18x18, and divide each of the i x 2 summations by the summation of the 20 x 2 left section, and each of the i x 18 summations by the summation of the 20 x 2 right section.  

No-till is represented by an identical matrix, $I$, after @cousensModelEffectsCultivation1990. A field cultivator was applied before planting corn (C2, C3, and C4), soybean (S2, S3, and S4), and oat (O3 and O4). No tillage was applied before alfalfa (A4) because alfalfa that was intercropped with oat in the 4-year rotation (O4) was kept overwinter and grown as a sole crop in the following year.     


$$
B_{t(s)} = \left[\begin{array}
{rr|rrrrr} 
t_{11,s} & t_{21,s} & 0 & ... & ... & ... & ... & 0\\
t_{12,s} & t_{22,s} & 0 & ... & ... & ... & ... & 0\\
\hline    
0 & 0 & 0 & ... & ... & ... & ... &0\\
... & ... & ... & ... & ... & ... & ... & ...\\ 
... & ... & ... & ... & ... & ... & ... & ...\\ 
0 & 0 & 0 & ... & ... & ... & ... & 0\\
\end{array}\right]
$$   


The same pre-planting tillage regimes were applied in 2018 and 2019. The list of pre-planting tillage matrices is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-pre-planting-tillage.RData>.    





##### In-season survival of seeds and seedlings {-}

The matrix $B_s$ is comprised of seed survival rates at the $M_s$ and plant survival rates at the $M_p$ sections, respectively.  


$$
B_s = \left[\begin{array}
{rr|rrrrr} 
s_{s_1}  & 0 & 0 & ... & ... & ... & ... & 0\\
0 & s_{s_2}  & 0 & ... & ... & ... & ... & 0\\
\hline             
0 & 0 & s_{p_1} & ... & ... & ...&...  & 0\\
... & ... & 0 & s_{p_2} & ... & ... & ... & 0\\ 
... & ... & ... & ... & ... & ... & ... & 0\\ 
0 & 0 & 0 & ... & ... & ...& 0 & s_{p_6}\\
\end{array}\right]
$$ 

The $M_s$ section's diagonal ($s_{s_1}$ and $s_{s_2}$) were filled with survival rates adapted from equations $o_1 = y = 98.9 e^{-0.068x} \%$ and $o_2 = y = 99.5 e^{-0.05x} \%$ [Figures 1 and 3, @sosnoskieGlyphosateResistanceDoes2013] for the top and bottom layers. The values of x was assigned at 6 months for all crop environments. We settled at 6 months despite the complexity in tillage timing and method, light and humidity conditions, and granivores' activities at individual crop environments for simplicity. In reality, the burial length can interact with any crop management activity and deliver different germination and emergence results.  


The empirically measured data for seedling survival were deemed unrealistically (Appendix) low as compared to the literature, so @nordbyInfluenceCornCommon2004's results were used for corn and @hartzlerEffectCommonWaterhemp2004's results were used for soybean crop environments. The seedling survival rates by cohort ($s_{p_k}, k = \{1,...,6\}$) were assigned such that the earlier cohorts had lower survival rate in the oat crop environment; and those in the alfalfa crop environment were evenly low in all cohorts. These estimated numbers were based on a suggestion that cool-season crop environments can inhibit warm-season weed species growth [@nguyenWeedCommunityComposition2022 and citations given there].

The same summer survival rates were used in 2018 and 2019. The list of summer seed survival rate matrices is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-summer-seed-survival-Sosnoskie.RData> and the list of seedling survival rate to maturity is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-summer-seedling-survival-Hartzler.RData>.      




##### Post-harvest tillage induced vertical redistribution of seeds post-harvest tillage {-}

The compilation of $B_{t(f)}$ was the similar to that of $B_{t(s)}$. Chisel plowing was applied after corn was harvested in the C2, C3, and C4 treatments, no-till was applied after harvests in the  S2, S3, S4, and O4 treatments, and moldboard plowing was applied at the end of the O3 and A4 phases.  

$$
B_{t(f)} = \left[\begin{array}
{rr|rrrrr} 
t_{11,f} & t_{21,f} & 0 & ... & ... & ... & ... & 0\\
t_{12,f} & t_{22,f} & 0 & ... & ... & ... & ... & 0\\
\hline    
0 & 0 & 0 & ... & ... & ... & ... & 0\\
... & ... & ... & ... & ... & ... & ... & ...\\ 
... & ... & ... & ... & ... & ... & ... & ...\\ 
0 & 0 & 0 & ... & ... & ... & ... & 0\\
\end{array}\right]
$$   

The same post-harvest tillage regimes were applied in 2018 and 2019. The list of pre-planting tillage matrices is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-post-harvest-tillage.RData>    



##### Overwinter survival {-} 

The compilation of matrix $B_o$ was similar to that of $B_s$, using equations $o_1 = y = 98.9 e^{-0.068x} \%$ and $o_2 = y = 99.5 e^{-0.05x} \%$ [Figures 1 and 3, @sosnoskieGlyphosateResistanceDoes2013].  

$$
\mathbf{B_o} = \left[\begin{array}
{rr|rrrrr} 
o_{11} & 0 & 0 & ... & ... & ... & ... & 0\\
0  & o_{22} & 0 & ... & ... & ... & ... & 0\\
\hline    
0 & 0 & 0 & ... & ... & ... & ... & 0 \\
... & ... & ... & ... & ... & ... & ... & ...\\ 
... & ... & ... & ... & ... & ... & ... & ...\\ 
0 & 0 & 0 & ... & ... & ... & ... & 0\\
\end{array}\right]
$$   

The same overwinter survival rates were used in 2018 and 2019. Some zero values were due to rounding. The list of overwinter seed survival matrices is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-winter-seed-survival-Sosnoskie.RData>     




#### Empirically measured data {-}

##### Seedling recruitment {-}  

The emergence proportions calculated from step 5 here are positioned on the first column of block $M_{s,p}$ in matrix $B_g$. $1 - sum_{k=1}^6 g_k$ represents the proportion of non-emerging seeds.  

$$
\mathbf{B_g}=\left[\begin{array}
{rr|rrrrr} 
d = 1-\sum_{k=1}^6 g_k & 0 & 0 & ... & ... & ... & ... & 0\\  
0 & 1 & 0 & ... & ... & ... & ... & 0\\  
\hline             
g_1 & 0 & 0 & ... & ... & ... & ... & 0\\  
... & ... & ... & ... & ... & ... & ... & ...\\  
g_6 & 0 & 0 & ... & ... & ... & ... & 0\\  
\end{array}\right]
$$   


The proportion of seedling emergence from the top 0-2 cm soil seedbank stratum in each crop identity crossed with corn weed management regime was calculated with the following steps:   

1 - Estimate the 0-2 cm and 2-20 cm seedbank densities with the soil seedbank samples collected before post-harvest tillage. A seed column at a particular sub-annual period is comprised of the 0-2 cm and 2-20 cm soil stratum seed densities, $N_h = [s_t, s_b]$.  

From steps 2 through 4, the seed column in sub-period h, $N_h$, was transitioned from one period to the next with the general matrix multiplication of $N_{h+1} = B_hN_h$ by @caswellMatrixPopulationModels2001.  

2 - Estimate post-harvest tillage induced seed vertical redistribution with resized Seed Chaser [@spokasSeedChaserVerticalSoil2007] chisel and moldboard plowing matrices, as detailed in the *Post-harvest tillage induced seed vertical movement*, to yield $N_{t(f)}$       

3 - Adapt overwinter survival rates as previously explain in he *Overwinter survival section* and apply it on $N_{t(f)}$ to yield $N_o$. Corn weed management did not affect waterhemp's first cohort emergence in the same crop environment (Appendix), so the same value of $x_s$ was used for the same crop identity.    

4 - Estimate pre-planting tillage induced seed vertical redistribution with resized Seed Chaser [@spokasSeedChaserVerticalSoil2007] field cultivator matrix, similar to step 2 to yield $N_{t(s)}$.   

5 - Divide the seedling density in each cohort, $l_k$, by $s_{11,f}$, the top soil stratum seed density to yield $g_k$.   

The same emergence rates were used in 2018 and 2019. Some zero values in the first column were due to rounding. The list of seedling recruitment matrices is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-emergence-prop-adjusted.RData>       




##### Plant fecundity {-}  

The plant fecundity matrix ($B_f$) had the $M_s$ block's diagonal filled with 1's and the first row of the $M_{p,s}$ filled with $f_k, k = \{1,...,6\}$. The 1's in the $M_s$ block's diagonal are placeholders to carry the product from the previous matrices over.  

$$
\mathbf{B_f} = \left[\begin{array}
{rr|rrrrr} 
1 & 0 & f_1 & f_2 & ... & ... & ... & f_6\\
0 & 1 & 0 & ... & ... & ... & ... & 0\\
\hline   
0 & 0 & 0 & ... & ... & ... & ... & 0\\
... & ...& ... & ... &...&...&...& 0\\
... & ... & ... & ... & ... & ... & ... & ...\\ 
0 & 0 & 0 & ... & ... & ... & 0 & 0\\
\end{array}\right]
$$   


Two scenarios of plant fecundity were used. In scenario 1, plant fecundity ($f_k, k =\{1,...,6\}$) in each crop identity crossed with corn weed management was estimated from plant aboveground mass using eighteen equations from @nguyenImpactCroppingSystem2022. In scenario 2, the plants were partitioned into six size-based bins and their fecundity was summarized as $f_k, k =\{1,...,6\}$ and filled in their relevant positions in the $B_f$ matrix by partitioning. Both practices in scenarios 1 and 2 were based on the assumption that plant size and fecundity decreased as emergence delayed [@hartzlerEffectCommonWaterhemp2004; @nordbyInfluenceCornCommon2004].   

Scenario 1: High control efficacy  

The list of cohort-averaged fecundity matrices under high control efficacy is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-fecundity-19-cohort.RData>




Scenario 2: Low control efficacy 

The list of cohort-averaged fecundity matrices under low control efficacy is available at <https://github.com/hnguyen19/matrix-prospective/blob/master/2-Data/Clean/mean-fecundity-18-cohort.RData>




