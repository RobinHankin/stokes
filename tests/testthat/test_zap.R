## Some tests of zap()

test_that("zap works as expected", {     

    checker1 <- function(a){
        expect_true(a == zap(a))
    }

    checker2 <- function(a,b,small=1e-100){
        expect_false(zap(a) == a*small)
        expect_false(zap(b) == b*small)

        expect_true(zap(a + small*b) == a)
        expect_true(zap(b + small*a) == b)
    }


    a <- as.kform(matrix(1:9,3,3))
    b <- as.kform(matrix(c(1,2,6,4,5,3,7,8,9),3,3))

    checker1(a)
    checker1(b)

    checker2(a,b)
    checker2(b,a)


    
    a <- as.ktensor(matrix(1:9,3,3))
    b <- as.ktensor(matrix(c(1,2,6,4,5,3,7,8,9),3,3))


    checker1(a)
    checker1(b)

    checker2(a,b)
    checker2(b,a)

})

