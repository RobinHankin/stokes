## Some tests of pullback()

options(warn=999)
test_that("Function pullback() behaves itself", {
  expect_true(TRUE)

  `getgood` <- function(randex, test=function(...){TRUE}, maxit=1e5, default, ...){
    it <- 0
    while(it < maxit){
      if(test(out <- eval(randex), ...)){return(out)}
      it <- it+1
    }
    if(missing(default)){
      stop("no good value found; try increasing maxit, or relaxing ftest()")
    } else {
      return(default)
    }
  }


    foo <- function(x,M){  # checks that pullback(pullback(x,M),solve(M)) == x
        xt <- pullback(pullback(x,M),solve(M))
        discrepancy <- x |> pullback(M) |> pullback(solve(M)) - x
        expect_true(issmall(discrepancy),info=list(x,M))
    } # foo() closes

    for(i in 1:3){
        o <-  rform(terms=3,k=2,n=5,coeffs=rnorm(3))
        randmat <- expression(matrix(rnorm(25),5,5))
        isbigdet <- function(x,min_det = 0.01){abs(det(x)) > min_det}
        M <- getgood(randmat, isbigdet, default=diag(5), min_det=0.1)
        foo(o,M)
        foo(o*0,M)
    }



})
