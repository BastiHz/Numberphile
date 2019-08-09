#' Sieve of Eratosthenes
#'
#' Get all prime numbers between 2 and n.
#'
#' @param n The upper limit of the region
#'
#' @return A vector of the primes between 2 and n.
#'
#' @examples
#' sieve_of_eratosthenes(100)
#'
#' @export
sieve_of_eratosthenes <- function(n) {
    primes <- rep(TRUE, n)
    primes[1] <- FALSE  # 1 is not prime
    for (i in 2:ceiling(sqrt(n))) {
        if (primes[i]) {
            primes[seq(i + i, n, i)] <- FALSE
        }
    }
    which(primes)
}
