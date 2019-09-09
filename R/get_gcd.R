#' Greates Common Divisor
#'
#' Recursively compute the greatest common divisor.
#'
#' @param a,b Single numbers.
#'
#' @examples
#' get_gcd(12, 6)
#'
#' @export
get_gcd <- function(a, b) {
    if (b == 0) a else get_gcd(b, a %% b)
}
