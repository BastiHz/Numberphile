context("sieve_of_eratosthenes")

test_that("the result is correct", {
    p <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
           67, 71, 73, 79, 83, 89, 97)
    expect_identical(sieve_of_eratosthenes(100), as.integer(p))
})
