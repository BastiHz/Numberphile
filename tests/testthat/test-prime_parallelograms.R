context("prime_parallelograms")


test_that("the result is correct", {
    foo <- tail(prime_parallelograms(10000, plot = FALSE))
    rownames(foo) <- NULL
    bar <- data.frame(
        primes = as.integer(c(9929, 9931, 9941, 9949, 9967, 9973)),
        y = as.integer(c(496, -3598, -1028, -2044, -5866, -1252)),
        i = 1224:1229
    )
    expect_identical(foo, bar)
})
