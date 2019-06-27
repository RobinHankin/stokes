
foo1 <- function(S){
  expect_true(S == +S,info=S)
  expect_true(S == -(-S),info=S)
  expect_true(S - S == S*0,info=S)
  expect_true(S + S == 2*S,info=S)
  expect_true(S + S == S*2,info=S)
  expect_true(S/2 + S/2 == S,info=S)
} # foo1() closes

foo2 <- function(S1,S2){
  expect_true(S2 + S1 - S2 == S1,info=list(S1,S2))
  expect_true(S1 + S2 == S2 + S1,info=list(S1,S2))
}  # foo2() closes

test_that("Cross product Ops behave", {
  for(i in 1:10){
    terms <- rpois(1,20)
    k <- sample(3:10,1)
    n <- k+sample(3:10,1)
    S <- rform(terms,k,n,sample(terms))
    foo1(S)
  }
  
  for(i in 1:10){
    terms <- rpois(1,20)
    k <- sample(3:10,1)
    n <- k+sample(3:10,1)
    S1 <- rform(terms,k,n,sample(terms))
    S2 <- rform(terms,k,n,sample(terms))
    foo2(S1,S2)
  }
  
})

test_that("Wedge product Ops behave", {
  
  for(i in 1:10){
    terms <- rpois(1,20)
    k <- sample(3:10,1)
    n <- k+sample(3:10,1)
    K <- rform(terms,k,n,sample(terms))
    foo1(K)
  }
  
  for(i in 1:10){
    terms <- rpois(1,20)
    k <- sample(3:10,1)
    n <- k+sample(3:10,1)
    K1 <- rform(terms,k,n,sample(terms))
    K2 <- rform(terms,k,n,sample(terms))
    foo2(K1,K2)
  }
})

      
