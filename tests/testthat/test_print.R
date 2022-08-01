## Some tests of the print methods

options(warn=999)
test_that("Functions print.ktensor() and print.kform() behave", {

  checker <- function(a){
    expect_output(print(a))
    expect_output(print(a*0))
  }

  n <- 2


  ##  check printing of ktensors:
  options("ktensor_symbolic_print" = FALSE)
  for(i in seq_len(n)){checker(rtensor())}
  
  options("ktensor_symbolic_print" = TRUE)
  for(i in seq_len(n)){checker(rtensor())}

  options("ktensor_symbolic_print" = FALSE)  # revert to default
  


  ## check printing of ktensors:
  options("kform_symbolic_print" = NULL)
  for(i in seq_len(n)){checker(rform())}

  options("kform_symbolic_print" = "dx")
  for(i in seq_len(n)){checker(rform())}

  options("kform_symbolic_print" = "txyz")
  for(i in seq_len(n)){checker(rform())}

  options("kform_symbolic_print" = "x") # generic non-null value
  for(i in seq_len(n)){checker(rform())}

  options("kform_symbolic_print" = FALSE)  # revert to default


})
