
# https://stackoverflow.com/questions/15795318/efficient-way-to-create-a-circulant-matrix-in-r

# create a circulant matrix from one vector
circ<-function(x) { 
  n<-length(x)
  matrix(x[matrix(1:n,n+1,n+1,byrow=T)[c(1,n:2),1:n]],n,n)
}

# trim off all-zero columns and rows
matrix_trim <- function(x) {
  m1 <- x == 0
  x[!(rowSums(m1)== ncol(m1)), 
    !(colSums(m1) == nrow(m1)),drop = FALSE]
}

