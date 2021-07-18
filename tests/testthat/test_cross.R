## Some tests of the cross product

test_that("cross product is distributive and associative", {

    foo3 <- function(T1,T2,T3){
        bar <- function(sbi){ # sbi = Should Be Identical
          for(i in 2:length(sbi)){
            error <- coeffs(sbi[[1]] - sbi[[i]])
            if(length(error)>0){
              expect_true(max(abs(error)) < 1e-8,
                          info=list(T1,T2,T3,error))
            } else {  # test passed
              expect_true(TRUE)
            }
            return(TRUE)
          }
        } # bar closes

        ## associative:
        bar(list(
            cross(T1,T2,T3)         ,
            cross(cross(T1,T2),T3)  ,
            cross(T1,cross(T2,T3))  ,
            T1 %X% T2 %X% T3        ,
            T1 %X% (T2 %X% T3)      ,
            (T1 %X% T2) %X% T3
        ))
        ## distributive:
        bar(list(
            cross(T1,T2 + T3)  ,
            T1 %X% (T2 + T3)   ,
            T1 %X% (T2 - (-T3)),
            T1 %X% T2 + T1 %X% T3
        ))
            
    } # foo3() closes

    for(i in 1:10){
      terms <- rpois(1,20)
      k <- sample(3:10,1)
      n <- k+sample(3:10,1)
      T1 <- rtensor(terms,k,n,rnorm(terms))
      T2 <- rtensor(terms+1,k,n,rnorm(terms+1))
      T3 <- rtensor(terms+2,k,n,rnorm(terms+2))
      foo3(T1,T2,T3)
    }
})

test_that("cross product is in fact a cross product", {

    foo2 <- function(T1,T2){

        E1 <- matrix(rnorm(k*n),n,k)
        E2 <- matrix(rnorm(k*n),n,k)

        jj1 <- as.function(T1)(E1)*as.function(T2)(E2)
        jj2 <- as.function(T1 %X% T2)(cbind(E1,E2))
        error <- jj1-jj2

        expect_true(max(abs(error)) < 1e-8,
                    info=list(T1,T2))
    } # foo2() closes

    for(i in 1:10){
      terms <- rpois(1,20)
      k <- sample(3:10,1)
      n <- k+sample(3:10,1)
      T1 <- rtensor(terms,k,n,rnorm(terms))
      T2 <- rtensor(terms+1,k,n,rnorm(terms+1))
      foo2(T1,T2)
    }
})

test_that("ktensors are multilinear", {

    foo1 <- function(TT){

        f <- as.function(TT)

        E <- matrix(rnorm(k*n),n,k)
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
            expect_true(abs(error) < 1e-8, info=list(TT,E1,E2,E3))
        } # 'i' loop closes, we have gone through each column
     } # foo1() closes


     for(i in 1:10){
       terms <- rpois(1,20)
       k <- sample(3:10,1)
       n <- k+sample(3:10,1)
       TT <- rtensor(terms,k,n,rnorm(terms))
       foo1(TT)
     }

})

