#' Chaos Polygon
#'
#' Start with the vertices of a regular polygon. Randomly place a tracepoint at
#' one of them. Each step of the iteration choose a random vertex and move the
#' tracepoint some distance towards that.
#'
#' @param vertices The number of vertices.
#' @param iterations The number of iterations.
#' @param proportion The proportion of the distance to move from the point to
#'   the target vertex.
#'
#' @references The Numberphile video featuring Ben Sparks and Brady Haran:
#'   \href{https://youtu.be/kbKtFN71Lfs}{Chaos Game}.
#'
#' @examples
#' p <- list(
#'     chaos_polygon(),
#'     chaos_polygon(4, 0.55),
#'     chaos_polygon(5, 0.62),
#'     chaos_polygon(6, 2/3),
#'     chaos_polygon(30, 0.91),
#'     chaos_polygon(proportion = 1.5)
#' )
#' par(mfrow = c(2, 3), mar = rep(0, 4))
#' for (xy in p) {
#'     plot(xy, pch = 20, axes = FALSE, ann = FALSE, asp = 1, cex = 0.25)
#' }
#'
#' @export
chaos_polygon <- function(vertices = 3,
                          proportion = 0.5,
                          iterations = 10000) {
    # TODO: Allow either a single integer for a regular polyhedron or an object
    # with an x and a y component as user-defined points. Remember to write that
    # down in the documentation.

    # Construct the regular polyhedron. Rotate the thing so that the bottom
    # edge is horizontal. It just looks nicer that way:
    radians <- seq(0, by = 2 * pi / vertices, length.out = vertices)
    radians <- radians - (pi / vertices) * (vertices - 1)
    vertices <- data.frame(x = sin(radians), y = cos(radians))

    tracepoint_x <- vertices$x[1]
    tracepoint_y <- vertices$y[1]
    result_x <- result_y <- numeric(iterations)
    targets <- sample(1:nrow(vertices), iterations, replace = TRUE)
    target_x <- vertices$x[targets]
    target_y <- vertices$y[targets]
    for (i in seq_len(iterations)) {
        tracepoint_x <- tracepoint_x - (tracepoint_x - target_x[i]) * proportion
        tracepoint_y <- tracepoint_y - (tracepoint_y - target_y[i]) * proportion
        result_x[i] <- tracepoint_x
        result_y[i] <- tracepoint_y
    }
    data.frame(x = result_x, y = result_y)
}


#' Chaos Fern
#'
#' Create a fractal that resembles a fern.
#'
#' The builtin parameter sets are "barnsley", "thelypteridaceae" and
#' "leptosporangiate". The names can be abbreviated. The barnsley fern is the
#' one shown in the video. The two others are taken from the examples on
#' \href{https://en.wikipedia.org/wiki/Barnsley_fern}{wikipedia}.
#'
#' @param params Either the name of a builtin parameter set (see details) or a
#'   data fram with the columns a, b, c, d, e and p.
#' @param iterations The number of iterations.
#'
#' @references The Numberphile video featuring Ben Sparks and Brady Haran:
#'   \href{https://youtu.be/kbKtFN71Lfs?t=338}{Chaos Game} (skip to 5:38).
#'
#' @examples
#' ferns <- list(
#'     chaos_fern(iterations = 30000),
#'     chaos_fern("thelypteridaceae", iterations = 30000),
#'     chaos_fern("leptosporangiate", iterations = 30000)
#' )
#' par(mfrow = c(1, 3), mar = rep(0, 4))
#' for (fern in ferns) {
#'     plot(fern, pch = ".", axes = FALSE, ann = FALSE, asp = 1, col = "forestgreen")
#' }
#'
#' @export
chaos_fern <- function(
        params = c("barnsley", "thelypteridaceae", "leptosporangiate"),
        iterations = 10000) {
    if (is.data.frame(params)) {
        stopifnot(ncol(params) == 7)
        if (any(names(params) != c(letters[1:6], "p"))) {
            stop("Colnames must be c('a' 'b' 'c' 'd' 'e' 'f' 'p').")
        }
    } else {
        params = match.arg(params)
        if (params == "barnsley") {
            params <- data.frame(
                a = c(0, 0.85, 0.2, -0.15),
                b = c(0, 0.04, -0.26 , 0.28),
                c = c(0, -0.04 , 0.23, 0.26),
                d = c(0.16, 0.85, 0.22, 0.24),
                e = c(0, 0, 0, 0),
                f = c(0, 1.6, 1.6, 0.44),
                p = c(0.01, 0.85, 0.07, 0.07)
            )
        } else if (params == "thelypteridaceae") {
            params <- data.frame(
                a = c(0, 0.95, 0.035, -0.04 ),
                b = c(0, 0.005, -0.2, 0.2),
                c = c(0, -0.005, 0.16, 0.16),
                d = c(0.25, 0.93, 0.04, 0.04),
                e = c(0, -0.002, -0.09, 0.083),
                f = c(-0.4, 0.5, 0.02, 0.12),
                p = c(0.02, 0.84, 0.07, 0.07)
            )
        } else if (params == "leptosporangiate") {
            params <- data.frame(
                a = c(0, 0.85, 0.09, -0.09),
                b = c(0, 0.02, -0.28, 0.28),
                c = c(0, -0.02, 0.30, 0.30),
                d = c(0.25,  0.83, 0.11, 0.09),
                e = c(0, 0, 0, 0),
                f = c(-0.14, 1, 0.6, 0.7),
                p = c(0.02, 0.84, 0.07, 0.07)
            )
        }
    }
    params <- as.matrix(params)

    result <- matrix(0, nrow = iterations, ncol = 2)
    colnames(result) <- c("x", "y")
    tracepoint <- c(0, 0)
    param_row <- sample(
        seq_len(nrow(params)),
        iterations - 1,  # -1 because the first point is always (0, 0)
        replace = TRUE,
        prob = params[, "p"]
    )
    abcd_index <- match(c("a", "b", "c", "d"), colnames(params))
    ef_index <- match(c("e", "f"), colnames(params))
    for (i in seq_len(iterations - 1)) {
        k <- param_row[i]
        m <- matrix(params[k, abcd_index], nrow = 2, byrow = TRUE)
        tracepoint <- m %*% tracepoint + params[k, ef_index]
        result[i + 1, ] <- tracepoint
    }
    as.data.frame(result)
}
