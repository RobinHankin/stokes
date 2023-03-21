test_that("Miscellaneous kinner tests", {

  dt <- d(1)
  dx <- d(2)
  dy <- d(3)
  dz <- d(4)

  mink <- diag(c(1,-1,-1,-1)) # Minkowski metric


  numerical <- c(
      kinner(dt^dx,dy^dz),
      kinner(dt^dx,dt^dx),
      kinner(dt^dx,dy^dz,mink),
      kinner(dt^dx,dt^dx,mink)
  )
  
  theoretical <- c(0,1,0,-1)
  
  expect_true(all(abs(numerical-theoretical) < 1e-11))
  
  
  rm(dt,dx,dy,dz,mink)

} )
