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
