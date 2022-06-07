## Diagnose a gls model to assess the effects of crop identity and corn weed management on individual fecundity (number of seeds), from <https://github.com/hnguyen19/AMATA-fecundity/blob/master/5-Analysis/functions.R>
library(broom)

diag_seed <- function(data, tag){
  aug_data <- augment(data)
  fit_resid <-  ggplot(aug_data, aes(x =  .fitted, y = .resid)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm",  size = 0.5, se = FALSE) +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle(label = tag) + 
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  #labs(tag = tag, face = "bold")
  
  obs_resid <-  ggplot(aug_data, aes(x = log(Seed + 1) , y = .resid)) +
    geom_point(size = 3) +
    geom_smooth( method = "lm", size = 0.5, se = F) +
    xlab("Observation Number") + 
    ylab("Residuals") +
    ggtitle(label = "", subtitle ="Index plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  resid_h <-  ggplot(aug_data, aes( x = .resid)) +
    geom_histogram( aes(y = ..density..), binwidth = 0.4, color = "black", fill = "grey") +
    # geom_density() + 
    xlab("Residuals") + 
    ylab("Density") + 
    stat_function(fun = dnorm, color = "blue", size = 0.5) + 
    #  ggtitle(label = "", subtitle ="Histogram") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  qq <-  ggplot(aug_data, aes(sample = log(Seed+1))) +
    stat_qq(size = 3) + 
    stat_qq_line(color = "blue", size = 0.5) + 
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + 
    #  ggtitle(label = "", subtitle ="Q-Q plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  return((fit_resid / qq / obs_resid / resid_h)) 
  
}



# https://stackoverflow.com/questions/15795318/efficient-way-to-create-a-circulant-matrix-in-r

# Create a circulant matrix from one vector
circ<-function(x) { 
  n<-length(x)
  matrix(x[matrix(1:n,n+1,n+1,byrow=T)[c(1,n:2),1:n]],n,n)
}

# Trim off all-zero columns and rows
matrix_trim <- function(x) {
  m1 <- x == 0
  x[!(rowSums(m1)== ncol(m1)), 
    !(colSums(m1) == nrow(m1)),drop = FALSE]
}

