#' Greatest Common Divisor
#'
#' Compute the greatest common divisor.
#'
#' @param a,b Single integers.
#'
#' @return The greatest common divisor of \code{a} and \code{b}.
#'
#' @examples
#' gcd(12, 8)
#'
#' @export
gcd <- function(a, b) {
    stopifnot(
        "a and b must be integer" = all(c(a, b) == as.integer(c(a, b))),
        length(a) == 1,
        length(b) == 1
    )
    a <- abs(a)
    b <- abs(b)
    while (b > 0) {
        temp <- b
        b <- a %% b
        a <- temp
    }
    a
}


# Recursive solution:
# if (b == 0) a else gcd(b, a %% b)
# It's slower than the iterative one. And you must make sure to only check for
# integer arguments once and not when it's recursively calling itself. You can
# do this whith an extra argument that's TRUE the first time but always FALSE
# after that.
