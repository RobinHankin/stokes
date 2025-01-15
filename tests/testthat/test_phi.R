
test_that("phi() behaves as it should", {

    expect_true(as.function(phi(3))(as.matrix(e(2,5))) == 0)
    expect_true(as.function(phi(3))(as.matrix(e(3,5))) == 1)
    expect_true(as.function(phi(3))(as.matrix(e(4,5))) == 0)
  
    v <- sample(9)
    expect_true(phi(v) == Reduce(`%X%`,sapply(v,phi)))
                


})
