A <- matrix(data=c(-4,1,1,0,1,-4,0,1,1,0,-4,1,0,1,1,-4), nrow=4, ncol=4, byrow=T)
b <- -1*matrix(data=c(7.68+8.18, 7.19+7.56, 8.41+8.36, 8.33+7.99), nrow=4, ncol=1, byrow=FALSE)
b
solve(A,b)
