test_that("print methods work as expected", {

    checker <- function(a){
        expect_output(print(a))
        expect_output(print(a*0))
    }

    for(i in 1:10){
        checker(rform())
        checker(rtensor())
    }
})

