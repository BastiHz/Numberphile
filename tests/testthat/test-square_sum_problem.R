test_that("square_sum_problem works", {
    chain <- square_sum_problem(15)
    squares <- seq_len(floor(sqrt(max(chain) + max(chain) - 1)))[-1] ^ 2
    sums <- chain[-1] + chain[-length(chain)]
    expect_true(all(sums %in% squares))
})


test_that("square_sum_problem produces expected errors", {
    for (n in c(1:14, 18:22, 24)) {
        expect_error(square_sum_problem(n))
    }
})
