#' Sieve of Eratosthenes
#'
#' Get all prime numbers between 2 and n.
#'
#' @param n The upper limit of the region that is searched for primes.
#'
#' @return A vector of the primes between 2 and n.
#'
#' @examples
#' sieve_of_eratosthenes(100)
#'
#' @export
sieve_of_eratosthenes <- function(n) {
    stopifnot(
        is.numeric(n),
        n > 1
    )
    if (n < 6) {
        primes <- switch (n,
            NULL,
            2,
            c(2, 3),
            c(2, 3),
            c(2, 3, 5)
        )
        return(as.integer(primes))
    }
    primes <- rep(TRUE, n)
    primes[1] <- FALSE  # 1 is not prime
    for (i in 2:ceiling(sqrt(n))) {
        if (primes[i]) {
            primes[seq(i + i, n, i)] <- FALSE
        }
    }
    which(primes)
}
