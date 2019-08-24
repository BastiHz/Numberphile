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
#' @examples
#' ff <- forest_fire(5000)
#' plot(ff, pch = 16, col = "#00000070")
#'
#' @export
forest_fire <- function(n) {
    stopifnot(n > 0)
    if (n == 1) return(1)
    if (n == 2) return(c(1, 1))
    ff <- c(1, 1)
    for (k in seq(2, n-1)) {
        i <- seq(k, k/2+1, -1)
        j <- seq(k-1, 1, -2)
        forbidden <- unique(sort(ff[i] + ff[i] - ff[j]))
        forbidden <- forbidden[forbidden > 0]
        control <- seq_along(forbidden)
        different <- which(forbidden != control)
        if (length(different) > 0) {
            ff <- c(ff, min(different))
        } else {
            ff <- c(ff, max(forbidden) + 1)
        }
    }
    ff
}
