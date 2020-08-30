<<<<<<< HEAD
## Some tests of the print methods

options(warn=999)
test_that("Functions print.ktensor() and print.kform() behave", {
    expect_true(TRUE)

    fooprint <- function(x){expect_output(print(x))}
    for(i in 1:10){
      x <- rform()
      fooprint(x)
      fooprint(x*0)
      x <- rtensor()
      fooprint(x)
      fooprint(x*0)
    }
})
=======
test_that("print methods work as expected", {

    checker <- function(a){
        expect_output(print(a))
        expect_output(print(a*0))
    }

    for(i in 1:10){
        checker(rform())
        checker(rtensor())
    }
})

>>>>>>> refs/remotes/origin/master
