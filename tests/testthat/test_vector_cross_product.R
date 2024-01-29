test_that("Miscellaneous vector cross product tests", {
expect_true(all(abs(vector_cross_product(cbind(c(5,-2,1),c(1,2,0))) -c(-2,1,12)) < 1e-11))
expect_true(all(abs(vector_cross_product(cbind(c(5,-2)           )) -c( 2,5   )) < 1e-11))
} )
