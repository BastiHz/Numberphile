#' Balanced Ternary
#'
#' Write a number n in base 3, substitute every 2 with a -1, then convert to
#' decimal.
#'
#' @param n The number of terms of the sequence to generate.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A117966}{A117966}.
#'
#'   The Numberphile video featuring Neil Sloane and Brady Haran.
#'   \href{https://www.youtube.com/watch?v=o8c4uYnnNnc}{Amazing Graphs II
#'   (including Star Wars)}.
#'
#' @examples
#' bt <- balanced_ternary(200)
#' plot(bt$n, bt$y)
#'
#' @export
balanced_ternary <- function(n) {
    stopifnot(n > 0)
    n <- n - 1
    bt <- integer(n)
    for (i in seq_len(n)) {
        result = integer()
        j <- i
        while (j > 0) {
            result <- c(j %% 3, result)
            j <- j %/% 3
        }
        result[result == 2] <- -1
        balanced <- 0
        pow <- 0
        for (k in rev(result)) {
            balanced <- balanced + k * 3 ^ pow
            pow <- pow + 1
        }
        bt[i] <- balanced
    }
    data.frame(n = c(0, seq_len(n)), y = c(0, bt))
}
