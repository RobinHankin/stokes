## Some tests of the print methods

options(warn=999)
test_that("Functions print.ktensor() and print.kform() behave", {

  checker <- function(a){
    expect_output(print(a))
    expect_output(print(a*0))
  }
  
  for(i in 1:10){
    checker(rform())
    checker(rtensor())
  }
})
