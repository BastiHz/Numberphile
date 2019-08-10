#' Prime Parallelograms
#'
#' Take a prime number, convert it to binary, reverse it, convert it
#' back to decimal, and then substract it from the original prime number.
#'
#' @inheritParams sieve_of_eratosthenes
#' @param plot Logical. Should the parallelograms be plotted?
#'
#' @return Invisibly returns a data frame with columns for the primes, the
#'   result of the binary reversal and substraction, and the indices of the
#'   primes.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#' \href{https://oeis.org/A265326}{A265326}.
#'
#' The Numberphile video featuring Neil Sloane and Brady Haran.
#' \href{https://www.youtube.com/watch?v=pAMgUB51XZA&feature=youtu.be&t=467}{Amazing
#' Graphs}. Skip to 7:47 if the video plays from the beginning.
#'
#' @examples
#' prime_parallelograms(10000)
#'
#' @export
prime_parallelograms <- function(p_max, plot = TRUE) {
    bin_rev_dec_sub <- function(x) {
        y <- intToBits(x)
        y <- as.integer(y)
        y <- y[1:tail(which(y == 1), 1)]
        y <- paste(y, collapse = "")
        y <- strtoi(y, 2)
        x - y
    }
    p <- sieve_of_eratosthenes(p_max)
    y <- vapply(p, bin_rev_dec_sub, FUN.VALUE = 1L)
    i <- seq_along(p)  # In the video the indices of the primes are on the x axis

    if (plot) {
        plot(i, y, pch = 20, cex = 0.3, las = 1)
    }
    out <- data.frame(
        primes = p,
        y = y,
        i = i
    )
    invisible(out)
}
