## Some tests to increase coverage 

test_that("coverage of functionality in mult.R", {
     S <- spray(matrix(c(1,1,2,2,1,3,3,1,3,5),ncol=2,byrow=TRUE),1:5)
     expect_silent(include_perms(kill_trivial_rows(S)))

     expect_silent(cross(S,S*0))
     expect_silent(cross(S*0,S))
     expect_silent(cross(S*0,S*0))

     expect_silent(wedge(as.kform(S)))

     expect_silent(as.kform(matrix(1,0,3),lose=TRUE))
     expect_silent(as.kform(matrix(1,0,3),lose=FALSE))

     expect_error(as.symbolic(function(x){x^5}))

})
