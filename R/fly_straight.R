#' Fly Straight
#'
#' TODO: write a nice description
#'
#' @param n The number of terms of the sequence to generate.
#'
#' @examples
#' fs <- fly_straight(1000)
#' plot(fs)
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A133058}{A133058}.
#'
#'   The Numberphile video featuring Neil Sloane and Brady Haran.
#'   \href{https://www.youtube.com/watch?v=pAMgUB51XZA}{Amazing Graphs}.
#'
#' @export
fly_straight <- function(n) {
    stopifnot(n > 1)
    a <- c(1, 1, rep(NA, n-2))
    for (i in 2:n) {
        gcd <- get_gcd(i, a[i-1])
        if (gcd == 1) {
            a[i] <- a[i-1] + i + 1
        } else {
            a[i] <- a[i-1] / gcd
        }
    }
    data.frame(x = 0:n, y = c(1, a))
}
