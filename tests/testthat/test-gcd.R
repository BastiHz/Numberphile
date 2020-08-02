test_that("gcd works with integers", {
    expect_equal(gcd(12, 8), 4)
    expect_equal(gcd(17, 7), 1)
    expect_equal(gcd(3, 0), 3)
    expect_equal(gcd(0, 0), 0)
})

test_that("gcd works with negative integers", {
    expect_equal(gcd(-12, 8), 4)
    expect_equal(gcd(-12, -8), 4)
    expect_equal(gcd(12, -8), 4)
})

test_that("gcd fails for floats", {
    expect_error(gcd(17.5, 7), regexp = "a and b must be integer")
})
