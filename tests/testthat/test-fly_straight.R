context("fly_streight")


test_that("the sequence generates correctly", {
    # comparing it with the sequence from oeis
    fs <- fly_straight(68)
    fs <- rbind(head(fs, 3), tail(fs, 3))
    rownames(fs) <- NULL
    control <- data.frame(
        x = c(0, 1, 2, 66,67, 68),
        y = c(1, 1, 4, 266, 334, 167)
    )
    expect_equal(fs, control)
})
