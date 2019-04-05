test_that("wedge product is associative", {

    K1 <- as.kform(cbind(1:5,2:6),1:5)
    K2 <- as.kform(cbind(5:7,6:8,7:9),1:3)
    K3 <- kform_general(1:6,2)

    jj <- list(
        wedge(K1,K2,K3)         ,
        wedge(wedge(K1,K2),K3)  ,
        wedge(K1,wedge(K2,K3))  ,
        K1 %^% K2 %^% K3        ,
        K1 %^% (K2 %^% K3)      ,
        (K1 %^% K2) %^% K3
    )
    
    for(i in 2:length(jj)){
      expect_true(is.zero(jj[[1]] - jj[[i]]))
    }
})

      
