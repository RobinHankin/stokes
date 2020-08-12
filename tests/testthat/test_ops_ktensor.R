## Some tests to increase coverage 

test_that("coverage of Ops.ktensor()", {

    a <- as.ktensor(cbind(1:4,2:5,3:6),1:4)

    expect_silent(a)
    expect_silent(+a)
    expect_silent(-a)
    expect_error(!a)
    
    expect_true(a==a)
    expect_error(a==1)
    expect_error(1==a)    
    expect_false(a!=a)
    expect_error(a&a)
    expect_false(a != a)
    expect_silent(a*a)
    expect_error(a/a)
    expect_error(a + 4)
    expect_error(a - 4)


    expect_silent(a*7)
    expect_silent(7*a)

    expect_silent(a/7)
    expect_error(7/a)
    
})
