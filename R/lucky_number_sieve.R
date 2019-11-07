#' Lucky Number Sieve
#'
#' Get all lucky numbers <= n.
#'
#' @param n The upper limit of the region that is searched for lucky numbers.
#'
#' @return A vector of the lucky numbers between 1 and n.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A000959}{A000959}.
#'
#'   The Numberphile video featuring Ria Symonds and Brady Haran:
#'   \href{https://youtu.be/RxxDD2LWAyY}{What is a lucky number?}.
#'
#' @examples
#' lucky_number_sieve(100)
#'
#' @export
lucky_number_sieve <- function(n) {
    lucky_numbers <- seq(1, n, 2)
    for (i in 2:n) {
        step <- lucky_numbers[i]
        if (step > length(lucky_numbers)) {
            break
        }
        lucky_numbers <- lucky_numbers[-seq(step, n, step)]
    }
    lucky_numbers
}
