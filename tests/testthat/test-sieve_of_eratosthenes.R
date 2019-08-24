context("sieve_of_eratosthenes")


test_that("the sequence generates correctly", {
    p <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
           67, 71, 73, 79, 83, 89, 97)
    expect_identical(sieve_of_eratosthenes(100), as.integer(p))
})


test_that("p_max < 6 works", {
    expect_identical(sieve_of_eratosthenes(2), 2L)
    expect_identical(sieve_of_eratosthenes(3), c(2L, 3L))
    expect_identical(sieve_of_eratosthenes(4), c(2L, 3L))
    expect_identical(sieve_of_eratosthenes(5), c(2L, 3L, 5L))
})
