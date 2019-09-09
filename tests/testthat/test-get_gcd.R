context("get_gcd")


test_that("get_gcd works with integers", {
    expect_equal(get_gcd(12, 6), 6)
    expect_equal(get_gcd(17, 7), 1)
})

test_that("get_gcd works with other numbers", {
    expect_equal(get_gcd(-12, 6), 6)
    expect_equal(get_gcd(-12, -6), -6)
    expect_equal(get_gcd(17.5, 7), 3.5)
})
