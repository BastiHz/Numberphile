context("lucky_number_sieve")


test_that("the sequence generates correctly", {
    lucky <- lucky_number_sieve(303)
    lucky <- c(head(lucky, 3), tail(lucky, 3))
    control <- c(1, 3, 7, 289, 297, 303)
    expect_identical(lucky, control)
})
