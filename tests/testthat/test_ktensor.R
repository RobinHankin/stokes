test_that("ktensor functions are multilinear", {     

  `foo` <- function(S,E){
    f <- as.function(S)     
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
      expect_true(abs(error) < 1e-8)
    }  # 'i' loop closes
  } # foo() definition closes
  
  size <- 8  # how many tests to do
  no_of_rows <- 3   # number of rows in the ktensor
  
  for(i in seq_len(size)){
    for(n in 1:3){
      for(k in 1:4){
        ## Define a randomish k-tensor:
        M <- matrix(1+sample(no_of_rows*k)%%n,no_of_rows,k)
        S <- ktensor(spray(M, seq_len(no_of_rows),addrepeats=TRUE))

        ## And a random point in V^k:
        E <- matrix(rnorm(n*k),n,k)  

        ## Run the test:
        foo(S,E)
      }
    }
  }
})

test_that("ktensors are a vector space", {

    `foo2` <- function(S1,S2,E){
      
      f1 <- as.function(S1)
      f2 <- as.function(S2)
      
      a1 <- rnorm(1)
      a2 <- rnorm(1)
      
      f3 <- as.function(a1*S1 + a2*S2)
      
      error <- a1*f1(E) + a2*f2(E) - f3(E)
      expect_true(abs(error) < 1e-7)
    }
    
    for(k in 3:5){
      for(n in 4:6){
        for(u in 1:4){
          ## Define two randomish k-tensors:
          S1 <- ktensor(spray(matrix(1+sample(u*k)%%n,u,k),seq_len(u),addrepeats=TRUE))
          S2 <- ktensor(spray(matrix(1+sample(u*k)%%n,u,k),seq_len(u),addrepeats=TRUE))

          ## And a random point in V^k:
          E <- matrix(rnorm(n*k),n,k)  

          ## Run the test:
          foo2(S1,S2,E)
        }
      }
    }
})
