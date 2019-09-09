context("balanced_ternary")


test_that("the sequence generates correctly", {
    bt <- balanced_ternary(200)
    bt <- rbind(head(bt, 3), tail(bt, 3))
    rownames(bt) <- NULL
    control <- data.frame(
        x = c(0, 1, 2, 197, 198, 199),
        y = c(0, 1, -1, -58, -45, -44)
    )
    expect_identical(bt, control)
})


test_that("n = 1 works", {
    bt <- balanced_ternary(1)
    control <- data.frame(x = 0, y = 0)
    expect_identical(bt, control)
})
