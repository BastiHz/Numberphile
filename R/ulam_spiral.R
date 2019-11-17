#' Forest Fire
#'
#' Write the numbers from 1 to n in a square spiral and mark all primes.
#'
#' @param sidelength The side length of the spiral.
#'
#' @references The Numberphile video featuring James Grime and Brady Haran:
#'   \href{https://youtu.be/iFuR97YcSLM}{Prime Spirals}.
#'
#' @examples
#' us <- ulam_spiral(100)
#' grid::grid.raster(!us, interpolate = FALSE)
#'
#' @export
ulam_spiral <- function(sidelength) {
    stopifnot(sidelength > 1)
    n <- sidelength^2

    get_spiral_coordinates <- function(vec, a, b, j) {
        direction <- -1
        r <- 0
        while (length(vec) <= n) {
            a <- a + direction
            new <- b:a
            i <- j
            j <- i + length(new)
            vec[(i+1):j] <- new

            r <- r + 1
            new <- rep(a, r)
            i <- j
            j <- i + length(new)
            vec[(i+1):j] <- new

            temp <- a
            a <- b
            b <- temp
            direction <- direction * -1
        }
        vec <- vec[1:n]
    }

    x <- numeric(n)
    k <- floor((sidelength + 1) / 2)
    x[1:2] <- k:(k+1)
    x <- get_spiral_coordinates(x, k, k + 1, 2)

    y <- numeric(n)
    k <- ceiling((sidelength + 1) / 2)
    y[1] <- k
    y <- get_spiral_coordinates(y, k, k, 1)

    m <- matrix(0, nrow = sidelength, ncol = sidelength)
    m[cbind(y, x)] <- 1:n
    primes <- sieve_of_eratosthenes(n)
    matrix(m %in% primes, nrow = sidelength)
}
