## Some tests of coeffs and coeffs<-

options(warn=999)
test_that("Function hodge() behaves itself", {
    expect_true(TRUE)

    K <- kform(spray(matrix(c(1,2,3, 3,4,5, 1,3,5, 2,3,4),4,3,byrow=TRUE),1:4))
    expect_error(coeffs(K) <-  4:1)
    expect_error(coeffs(K)[coeffs(K)<3] <- coeffs(K)[coeffs(K)>2])
    expect_silent(coeffs(K)[coeffs(K)<3] <- coeffs(K)[coeffs(K)<3] + 100)
    K <- kform(spray(matrix(c(1,2,3, 3,4,5, 1,3,5, 2,3,4),4,3,byrow=TRUE),1:4))
    expect_silent(coeffs(K) <- 44)

    K <- ktensor(spray(matrix(c(1,2,3, 3,4,5, 1,3,5, 2,3,4),4,3,byrow=TRUE),1:4))
    expect_error(coeffs(K) <-  4:1)
    expect_error(coeffs(K)[coeffs(K)<3] <- coeffs(K)[coeffs(K)>2])
    expect_silent(coeffs(K)[coeffs(K)<3] <- coeffs(K)[coeffs(K)<3] + 100)
    K <- kform(spray(matrix(c(1,2,3, 3,4,5, 1,3,5, 2,3,4),4,3,byrow=TRUE),1:4))
    expect_silent(coeffs(K) <- 44)
    

} )
