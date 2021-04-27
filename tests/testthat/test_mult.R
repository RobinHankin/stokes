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

     expect_silent(as.symbolic(as.ktensor(+S)))
     expect_silent(as.symbolic(as.ktensor(-S)))

     expect_silent(as.symbolic(as.ktensor(+S*0)))
     expect_silent(as.symbolic(as.ktensor(-S*0)))

     expect_silent(as.ktensor(as.ktensor(S)))


     expect_silent(hodge(as.kform(S)))
     expect_error(hodge(0*volume(5)))
     expect_silent(hodge(volume(5)))

     expect_silent(hodge(0*volume(5),n=7))
     expect_error(hodge(0*volume(5),n=3))

     expect_error(hodge(scalar(6)))

     expect_silent(hodge(scalar(6),n=3))
     expect_silent(hodge(scalar(6),n=7))

     expect_silent(hodge(spray(rbind(c(1,2,4,5)))))

     expect_false(issmall(volume(5)))

     K <- as.kform(spray(matrix(c(1,1,2,2,1,3,3,1,3,5),ncol=2,byrow=TRUE),1:5))
     expect_error(stretch(K))
     expect_silent(stretch(K,1:5))

     expect_silent(keep(kform_general(7,3),1:4))
     expect_silent(discard(kform_general(7,3),1))

     expect_silent(zerotensor(5))
     expect_error(zerotensor(-5))

     expect_silent(as.kform(K))
     expect_silent(as.ktensor(K))


})
