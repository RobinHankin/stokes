test_that("wedge product is associative", {

    k1 <- as.kform(cbind(1:5,2:6),1:5)
    k2 <- as.kform(cbind(5:7,6:8,7:9),1:3)
    k3 <- kform_general(1:6,2)

    a1 <- k1 %^% (k2 %^% k3)
    a2 <- (k1 %^% k2) %^% k3

    expect_true(is.empty(a1-a2))
})
                
      
