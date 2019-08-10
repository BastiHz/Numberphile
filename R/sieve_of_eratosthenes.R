#' Sieve of Eratosthenes
#'
#' Get all prime numbers between 2 and n.
#'
#' @param p_max The upper limit of the region that is searched for primes.
#'
#' @return A vector of the primes between 2 and n.
#'
#' @examples
#' sieve_of_eratosthenes(100)
#'
#' @export
sieve_of_eratosthenes <- function(p_max) {
    stopifnot(
        is.numeric(p_max),
        p_max > 1
    )
    if (p_max < 6) {
        primes <- switch (p_max,
            NULL,
            2,
            c(2, 3),
            c(2, 3),
            c(2, 3, 5)
        )
        return(as.integer(primes))
    }
    primes <- rep(TRUE, p_max)
    primes[1] <- FALSE  # 1 is not prime
    for (i in 2:ceiling(sqrt(p_max))) {
        if (primes[i]) {
            primes[seq(i + i, p_max, i)] <- FALSE
        }
    }
    which(primes)
}
