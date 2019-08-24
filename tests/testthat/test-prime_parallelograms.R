context("prime_parallelograms")


test_that("the sequence generates correctly", {
    pp <- prime_parallelograms(10000, plot = FALSE)
    pp <- rbind(head(pp, 3), tail(pp, 3))
    rownames(pp) <- NULL
    control <- data.frame(
        primes = as.integer(c(2, 3, 5, 9949, 9967, 9973)),
        y = as.integer(c(1, 0, 0, -2044, -5866, -1252)),
        i = as.integer(c(1, 2, 3, 1227:1229))
    )
    expect_identical(pp, control)
})
