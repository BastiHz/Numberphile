context("forest_fire")


test_that("the sequence generates correctly", {
    ff <- forest_fire(200)
    ff <- c(head(ff, 3), tail(ff, 3))
    control <- c(1, 1, 2, 24, 20, 24)
    expect_identical(ff, control)
})
