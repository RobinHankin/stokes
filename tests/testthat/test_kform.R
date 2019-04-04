test_that("kform functions are multilinear", {

     size <- 5  # how many tests to do

     `foo` <- function(v){
       n <- v[1]
       k <- v[2]

       K <- kform_general(n,k,choose(n,k))
       f <- as.function(K)
     
       ## A random point in V^k:
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
       } # 'i' loop closes, we have gone through each column
     }
     apply(which(lower.tri(matrix(0,size,size)),arr.ind=TRUE),1,foo)
})
  

test_that("kform functions are alternating", {

  size <- 5  # how many tests to do
   
  `foo` <- function(v){
       n <- v[1]
       k <- v[2]
       K <- kform_general(n,k,choose(n,k))
     
       ## A random point in V^k:
       E <- matrix(rnorm(n*k),n,k)

       ## reverse the columns of E:
       Edash <- E[,rev(seq_len(k)),drop=FALSE]
       
       f <- as.function(K)

       sign_of_k <- ifelse(k%%4 %in% c(2,3),-1,+1)
       error <- f(E) - f(Edash)*sign_of_k
       expect_true(abs(error) < 1e-7)
  }

     apply(which(lower.tri(matrix(0,size,size)),arr.ind=TRUE),1,foo)
  })
