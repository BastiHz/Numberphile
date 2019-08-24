# https://oeis.org/A229037
# https://youtu.be/o8c4uYnnNnc?t=217


#' Forest Fire
#'
#' Sequence of positive integers where each is chosen to be as small as possible
#' subject to the condition that no three terms a(j), a(j+k), a(j+2k) (for any j
#' and k) form an arithmetic progression.
#'
#' @param n The number of terms of the sequence to generate.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A229037}{A229037}.
#'
#'   The Numberphile video featuring Neil Sloane and Brady Haran.
#'   \href{https://youtu.be/o8c4uYnnNnc?t=217}{Amazing Graphs II
#'   (including Star Wars)} (skip to 3:37).
#'
#' @note This implementation is quite slow. Be cautious with n > 10000.
#'
#' @examples
#' ff <- forest_fire(2000)
#' plot(ff)
#'
#' @export
forest_fire <- function(n) {
    stopifnot(n > 0)
    s <- numeric()
    for (k in seq_len(n)) {
        i <- j <- 1
        b <- numeric()
        while (k - 2 * i > 0) {
            b_new <- 2 * s[k-i] - s[k-2*i]
            if (!b_new %in% b) {
                b <- c(b, b_new)
            }
            i <- i + 1
            while (j %in% b) {
                b <- b[b != j]
                j <- j + 1
            }
        }
        s <- c(s, j)
    }
    s
}

# Implementation adapted from a python code example at https://oeis.org/A229037.
# TODO: Make it faster.
