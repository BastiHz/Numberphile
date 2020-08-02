test_that("ulam_spiral works without errors", {
    expect_error(ulam_spiral(15), NA)  # odd side length
    expect_error(ulam_spiral(16), NA)  # even side length
})
