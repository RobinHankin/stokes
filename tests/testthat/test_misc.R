
test_that("Miscellaneous wedge functionality", {

        dx <- as.kform(1)
        dy <- as.kform(2)
        dz <- as.kform(3)

        expect_true(is.empty(dx ^  0))
        expect_true(is.empty(0  ^ dx))

        expect_true(arity(dx)                     ==1)
        expect_true(arity(dx ^ dx)              ==2)
        expect_true(arity(dx ^ dx ^ dx)       ==3)
        expect_true(arity(dx ^ dx ^ dx ^ dx)==4)
        
        expect_true(is.empty(Alt(0  ^ dx)))
        expect_true(is.empty(Alt(dx ^  0)))

        expect_true(is.empty(dx ^ dx))
        expect_true(is.empty(dy ^ dy))
        expect_true(is.empty(dz ^ dz))
      
        expect_true(dx ^ dy ^ dz == dy ^ dz ^ dx)
        expect_true(dx ^ dy ^ dz == dz ^ dx ^ dy)

        expect_true(is.empty((dx ^ dx) ^ (dy ^ dz)))
        expect_true(is.empty((dy ^ dz) ^ (dx ^ dx)))

        expect_true(is.empty(dx ^ dy ^ dz - dy ^ dz ^ dx))
        expect_true(is.empty(dx ^ dy ^ dz - dz ^ dx ^ dy))
        expect_true(is.empty(dx ^ dy ^ dz + dx ^ dz ^ dy))

        expect_true(is.empty(dx ^ dy ^ dz ^ dx))

        expect_true( (dx ^ dy) ^ dz == dx ^ (dy ^ dz))

        expect_true(dx ^ dy ^ dz + dx ^ dy ^ dx == dx ^ dy ^ dz)

        expect_true(nrow(index(kform_general(4,2))) == 6)
        expect_true(kform_general(4,2) + dx ^ dx == kform_general(4,2))

        expect_true(is.empty(grad(1:5) ^ grad(1:5)))
        expect_false(is.empty(grad(1:5) ^ grad(1:6)))


        expect_true(as.function(0*rtensor(k=3,n=7))(matrix(rnorm(21),ncol=3))==0)
        expect_true(as.function(as.ktensor(0*rform(k=3,n=7)))(matrix(rnorm(21),ncol=3))==0)

})


test_that("Miscellaneous cross product functionality", {
        expect_true(is.empty(as.ktensor(1+diag(5),0)))
        expect_false(is.empty(as.ktensor(1+diag(5))))

        a  <- as.ktensor(matrix(1,1,2))
        a0 <- as.ktensor(matrix(1,1,2))

        expect_true(arity(a )==2)
        expect_true(arity(a0)==2)
        expect_true(arity(a  %X% a   )==4)
        expect_true(arity(a  %X% a0  )==4)
        expect_true(arity(a0 %X% a   )==4)
        expect_true(arity(a0 %X% a0  )==4)
        
})

test_that("as.symbolic() functionality", {
        expect_true(is.character(as.symbolic(rtensor())))
        expect_true(is.character(as.symbolic(rform())))
})


