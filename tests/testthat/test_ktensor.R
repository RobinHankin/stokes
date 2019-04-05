test_that("ktensor functions are multilinear", {     

     size <- 8  # how many tests to do
     no_of_rows <- 3   # number of rows in the ktensor

     `foo` <- function(v){
       n <- v[1]
       k <- v[2]

        
       ## Define a randomish k-tensor:
       M <- matrix(1+sample(no_of_rows*k)%%n,no_of_rows,k)
       S <- ktensor(spray(M, seq_len(no_of_rows),addrepeats=TRUE))
       f <- as.function(S)     

       ## And a random point in V^k:
       E <- matrix(rnorm(n*k),n,k)  
     
       ## change one column at a time:
       for(i in seq_len(ncol(E))){

     
         E1 <- E2 <- E3 <- E
         
         x1 <- rnorm(n)
         x2 <- rnorm(n)
         r1 <- rnorm(1)
         r2 <- rnorm(1)
         
         E1[,i] <- x1
         E2[,i] <- x2
         E3[,i] <- r1*x1 + r2*x2
         

         error <- r1*f(E1) + r2*f(E2) -f(E3) # should be small
         expect_true(abs(error) < 1e-7)
       }  # 'i' loop closes
     } # foo() definition closes
     apply(expand.grid(seq_len(size),seq_len(size)),1,foo)
     
     })
  

test_that("ktensors are a vector space", {

     k <- 4
     n <- 5
     u <- 3

     ## Define two randomish k-tensors:
     S1 <- ktensor(spray(matrix(1+sample(u*k)%%n,u,k),seq_len(u)))
     S2 <- ktensor(spray(matrix(1+sample(u*k)%%n,u,k),seq_len(u)))

     ## And a random point in V^k:
     E <- matrix(rnorm(n*k),n,k)  

     a1 <- rnorm(1)
     a2 <- rnorm(1)
     
     f1 <- as.function(S1)
     f2 <- as.function(S2)
     f3 <- as.function(a1*S1 + a2*S2)

     error <- a1*f1(E) + a2*f2(E) - f3(E)
     expect_true(abs(error) < 1e-7)
})
