#' Prime Parallelograms
#'
#' Take a prime number, convert it to binary, reverse it, convert it back to
#' decimal, and then substract it from the original prime number.
#'
#' @inheritParams sieve_of_eratosthenes
#' @param plot Logical. Should the parallelograms be plotted?
#'
#' @return A data frame with columns for the primes, the result of the binary
#'   reversal and substraction, and the indices of the primes.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A265326}{A265326}.
#'
#'   The Numberphile video featuring Neil Sloane and Brady Haran:
#'   \href{https://youtu.be/pAMgUB51XZA?t=467}{Amazing Graphs} (skip to 7:47).
#'
#' @examples
#' pp <- prime_parallelograms(10000)
#' plot(pp$i, pp$y, pch = 20, cex = 0.3)
#'
#' @export
prime_parallelograms <- function(p_max, plot = FALSE) {
    p <- sieve_of_eratosthenes(p_max)
    y <- lapply(p, intToBits)  # The output of intToBits() is already reversed.
    y <- lapply(y, as.integer)
    y <- lapply(y, paste, collapse = "")
    y <- sub("[0]+$", "", y)
    y <- p - strtoi(y, 2)
    i <- seq_along(p)
    if (plot) {
        # In the video and on OEIS the indices of the primes are on the x axis,
        # not the primes themselves.
        plot(i, y, pch = 20, cex = 0.3, las = 1)
    }
    data.frame(primes = p, y = y, i = i)
}
