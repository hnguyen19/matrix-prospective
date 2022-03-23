# <https://pdixon.stat.iastate.edu/stat534/R/matrix.r>

# matrix.r:  R code for matrix population models

a <- matrix(c(0, 2.6, 2.6, 0.2, 0, 0 , 0,   0.57, 0), nrow=3, byrow=T);
# by default, R wants numbers going down a column, 
#  the byrow=T tells R that numbers go across;

a

# another way to create the matrix:
# start with 3 vectors, one for each row, then stack them using rbind() "row bind"
a <- rbind(c(0, 2.6, 2.6), c(0.2, 0, 0), c(0, 0.57, 0))
a

# compute eigen values and associated right eigenvectors of A
ev <- eigen(a);

# print the modulus of the eigenvalues
print(Mod(ev$values))

# R is nice!  ev's are sorted in decreasing order
ev$values[1]   
  # eval vector is stored as complex numbers because some values are complex

l <- Re(ev$values[1])	# lambda, convert to real
u <- Re(ev$vectors[,1])	# and associated eigenvector, as real
u <- u/sum(u);		# standardize to unit sum = stable age dn

# then repeat for transpose of a to get left eigen vectors
# t(a) is the matrix transpose function

ev <- eigen(t(a))

# extract the reproductive values and standardize to sum to 1
v <- Re(ev$vectors[,1])
v <- v/sum(v)

# the other common standardization of repro. values is that the first stage 
#  has repro value = 1
v <- Re(ev$vectors[,1])
v <- v/v[1]

# sensitivity matrix
s <- outer(v,u) / sum(v*u);
round(s,2)

# outer() is the outer product of two vectors
outer(1:3, 1:3)
# round() rounds numbers to the specified decimal place

# elasticity matrix
e <- a * s / l
# * and / on a matrix do element-by-element multiplication and division
round(e, 2)

# Computing var lambda - need to convert sensitivy matrix to a vector
# here I am NOT eliminating the matrix elements with 0 variance

#  c(s) will convert a matrix to a vector going DOWN the columns
# to check, try this:

m <- matrix(1:9, nrow=3, byrow=T)
m
c(m)

# when a vector is used in a matrix operation, it is considered a column vector
# t() will transpose that to a row vector

t(c(m))

# and if R fusses that you really need a column vector, force it to a matrix with 1 column
matrix(c(m), ncol=1)

# construct a variance-covariance matrix
#  since most elements are 0, easier to create a matrix of all 0's,
#  then put in the non-zero values

v <- matrix(0, nrow=9, ncol=9) #Huong: another way to extract this vector v?

# second row/col is a21, 
v[2,2] <- 0.0036

# 6th row/col is a32
v[6,6] <- 0.0032

# 4th row/col is a12 = fecundity
v[4,4] <- 0.27

# 7th row/col is a13, also = fecundity
v[7,7] <- 0.27

# the two fecundities have correl=1, so cov = 0.27
# that's a correlation between the 4'th and 7'th values
v[7,4] <- v[4,7] <- 0.27

# variance of lambda
vl <- t(c(s)) %*% v %*% c(s) #column vector of Sensitivity

# and se of lambda
sqrt(vl)

# ---------------------------------------------------

# Here is how I computed the illustration very early in this block
#  of material

n <- c(0, 10, 10); # population starts with 10 age 1 adults and 10 age 2 adults

n <- a %*% n;	# the %*% operator is matrix multiply in R
print(n);

n <- a %*% n;
print(n);


# to follow for 15 years and save the results each year
nall <- matrix(0,nrow=3,ncol=15);
  # create a matrix with 3 zeros, 15 columns, filled with 0's 

n <- c(0, 10, 10);
for (i in 1:15) {
  nall[,i] <- n;
  n <- a %*% n;
  }

print(nall);

# calculate the total population size each "year"
#  rows are stages, columns are years
#  3rd argument is the function to apply to each column
#  2nd argument chooses columns (if 1, would apply to rows)
nsum <- apply(nall,2,sum);

print(nsum);

# convert counts to fraction, by dividing by column sum
#  if you're proficient in R, you realize there is a faster way
#  to do this, but it requires a bit of care

pall <- apply(nall,2,function(x){x/sum(x)});
pall









