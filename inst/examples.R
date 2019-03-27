n <- 5
k <- 3
E <- matrix(runif(n*k),n,k)  # random member of (R^n)^k
E


k <- 4
n <- 5
no_of_terms <- 3
S <- as.ktensor(spray(matrix(1+sample(no_of_terms*k)%%n,no_of_terms,k),0.5+seq_len(no_of_terms)))
E <- matrix(rnorm(n*k),n,k)   # random point in V^k

## Test multilinearity
E1 <- E
E2 <- E
E3 <- E

x1 <- rnorm(n)
x2 <- rnorm(n)
r1 <- rnorm(1)
r2 <- rnorm(1)

E1[,2] <- x1
E2[,2] <- x2
E3[,2] <- r1*x1 + r2*x2

f <- as.function(S)

print(paste("error is: ", (r1*f(E1) + r2*f(E2)) -f(E3))) # should be small

S1 <- spray(1+(magic(8)%%5)[1:5,2:5],0.5+1:5)
S1[rbind(1:4)] <- 3.1
S2 <- spray(matrix(c(1,2,2,1),2,2),c(10.1,11.1))

S3 <-
    spray(matrix(c(
        1,2,3,4,
        1,2,4,3,
        1,2,1,3,
        1,1,3,2,
        1,1,1,1,
        1,1,2,1,
        1,2,5,3
    ), ncol=4,byrow=TRUE),
    rnorm(7))

cross(S1,S2)

## In Relativity, we have things like
Alt(spray(expand.grid(1:4,1:4),rnorm(16)))

M <- matrix(c(
    1,2,
    1,3,
    1,4,
    2,3,
    2,4,
    3,4
),
ncol=2,byrow=TRUE)


    
k1 <- general_kform(1:4,3,1:4)  
k2 <- general_kform(6:9,2,10:15)
wedge(k1,k2)

k3 <- general_kform(1:4,3,c(1,0,0,0))
k4 <- general_kform(6:9,2,c(1,0,0,0,0,0))
wedge(k3,k4)

E <- matrix(rnorm(12),4,3)
as.function(k1)(E)


dx <- kform(1)
dy <- kform(2)
dz <- kform(3)

O <- wedge(dx,dy,dz)
f <- as.function(O)
M <- matrix(rnorm(9),3,3)
det(M) - f(M)  # should be small
