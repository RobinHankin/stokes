<<<<<<< HEAD
## Some tests of zap()

options(warn=999)
test_that("Function zap() behaves itself", {
    expect_true(TRUE)

    fooform2 <- function(x,y){  # checks that zap() works
      if(is.zero(x %^% y)){return(TRUE)}
      expect_false(x+1e-100*y == x)
      expect_false(x*1e-100+y == y)
      expect_true(zap(x+1e-100*y) == x)
      expect_true(zap(x*1e-100+y) == y)

    }  # fooform2() closes

    footensor2 <- function(x,y){
      if(x==y){return(TRUE)}
      
      expect_false(x+1e-100*y == x)
      expect_false(x*1e-100+y == y)
      expect_true(zap(x+1e-100*y) == x)
      expect_true(zap(x*1e-100+y) == y)
    } # footensor2() closes


    for(i in 1:10){
      x <- rform()
      y <- rform()
      fooform2(x,y)
      x <- rtensor()
      y <- rtensor()
      footensor2(x,y)

    }
})
=======
test_that("zap works as expected", {     


    checker1 <- function(a){
        expect_true(a == zap(a))
    }

    checker2 <- function(a,b,small=1e-100){
        expect_false(zap(a) == a*small)
        expect_false(zap(b) == b*small)

        expect_true(zap(a + small*b) == a)
        expect_true(zap(b + small*a) == b)
    }

    a <- as.kform(matrix(1:9,3,3))
    b <- as.kform(matrix(c(1,2,6,4,5,3,7,8,9),3,3))

    checker1(a)
    checker1(b)

    checker2(a,b)
    checker2(b,a)
    



    
    a <- as.ktensor(matrix(1:9,3,3))
    b <- as.ktensor(matrix(c(1,2,6,4,5,3,7,8,9),3,3))


    checker1(a)
    checker1(b)

    checker2(a,b)
    checker2(b,a)
    



})

>>>>>>> refs/remotes/origin/master
