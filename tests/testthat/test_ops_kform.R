## Some tests to increase coverage

test_that("coverage of Ops.kform()", {

    a <- as.kform(cbind(1:5,2:6),1:5)
    expect_error(!a)
    expect_error(a&a)
    expect_false(a != a)
    expect_error(a*a)
    expect_error(a/a)
    expect_error(a + 4)
    expect_error(a - 4)
    expect_error(1/a)
    expect_error(1==a)
})
