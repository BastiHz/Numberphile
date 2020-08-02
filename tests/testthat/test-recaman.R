test_that("the sequence generates correctly", {
    # comparing it with the sequence from oeis
    r <- recaman(71)
    r <- c(head(r, 3), tail(r, 3))
    control <- c(0, 1, 3, 156, 225, 155)
    expect_equal(r, control)
})
