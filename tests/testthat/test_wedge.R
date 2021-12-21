
test_that("wedge product is associative", {
  
  foo <- function(K1,K2,K3){
    
  jj <- list(
      wedge(K1,K2,K3)         ,
      wedge(wedge(K1,K2),K3)  ,
      wedge(K1,wedge(K2,K3))  ,
      K1 ^ K2 ^ K3        ,
      K1 ^ (K2 ^ K3)      ,
      (K1 ^ K2) ^ K3
  )
  for(i in 2:length(jj)){
    discrepancy <- jj[[1]] - jj[[i]]
    if(!is.zero(discrepancy)){
      error <- coeffs(discrepancy)
      expect_true(max(abs(error))<1e-8, info=list(K1,K2,K3,discrepancy))
    } else {
      expect_true(TRUE) 
    }
  }
  return(TRUE)
}  # foo() closes

foo(K1 = as.kform(cbind(1:5,2:6),1:5),
    K2 = as.kform(cbind(5:7,6:8,7:9),1:3),
    K3 = kform_general(1:6,2)
    )

foo(as.kform( matrix(c( 1, 6, 7, 2, 3, 6, 2, 4, 6),
    byrow=TRUE,ncol=3), coeffs = c(1,-1,-1) ),as.kform( matrix(c(
    1,2,7), byrow=TRUE,ncol=3), coeffs = -1 ), as.kform( matrix(c(
    1,2,3, 3,5,6, 1,4,5, 3,4,5), byrow=TRUE,ncol=3), coeffs =
    c(1,-1,-1,1) ))

for(i in 1:10){
  foo(rform(),rform(),rform())
}

expect_error(rform(  ) ^ rtensor())
expect_error(rtensor() ^ rform()  )
expect_error(rtensor() ^ rtensor())


})

      
