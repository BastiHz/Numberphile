# TODO: Change param "n_vertices" to "vertices". Allow either a single integer
# for a regular polyhedron or an object with an x and a y component as
# user-defined points.


#' Chaos Polygon
#'
#' Start with the vertices of a regular polygon. Randomly place a tracepoint at
#' one of them. Each step of the iteration choose a random vertex and move the
#' tracepoint some distance towards that.
#'
#' @param n_vertices The number of vertices.
#' @param n_iterations The number of iterations.
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
chaos_polygon <- function(n_vertices = 3,
                          proportion = 0.5,
                          n_iterations = 10000) {
    # Construct the regular polyhedron. Rotate the thing so that the bottom
    # edge is horizontal. It just looks nicer that way:
    radians <- seq(0, by = 2 * pi / n_vertices, length.out = n_vertices)
    radians <- radians - (pi / n_vertices) * (n_vertices - 1)
    vertices <- data.frame(x = sin(radians), y = cos(radians))

    # This version of the code below came to be after many iterations of
    # preformance improvements. At first I did everything using data frames
    # which took around 1 s for 1000 points. But using vectors like shown here
    # and creating the result data frame afterwards makes everything extremely
    # fast. Now it runs 1 million points in less than half a second. The new
    # bottleneck with so many points is the plot() function.
    tracepoint_x <- vertices$x[1]
    tracepoint_y <- vertices$y[1]
    result_x <- result_y <- numeric(n_iterations)
    targets <- sample(1:n_vertices, n_iterations, replace = TRUE)
    target_x <- vertices$x[targets]
    target_y <- vertices$y[targets]
    for (i in seq_len(n_iterations)) {
        tracepoint_x <- tracepoint_x - (tracepoint_x - target_x[i]) * proportion
        tracepoint_y <- tracepoint_y - (tracepoint_y - target_y[i]) * proportion
        result_x[i] <- tracepoint_x
        result_y[i] <- tracepoint_y
    }
    data.frame(x = result_x, y = result_y)
}

# Chaos Fern
# chaos_fern <- function(n_iterations = 10000, params = "barnsley",
#                        plot = TRUE, cex = 0.25, ...) {
#     # params = Either the string "barnsley" or a data frame or matrix
#     #     containing the parameters a, b, c, d, e, f and p.
#     if ("character" %in% class(params) && params == "barnsley") {
#         params <- data.frame(
#             a = c(0, 0.85, 0.2, -0.15),
#             b = c(0, 0.04, -0.26 , 0.28),
#             c = c(0, -0.04 , 0.23, 0.26),
#             d = c(0.16, 0.85, 0.22, 0.24),
#             e = c(0, 0, 0, 0),
#             f = c(0, 1.6, 1.6, 0.44),
#             p = c(0.01, 0.85, 0.07, 0.07)
#         )
#     }
#     params <- as.matrix(params)
#
#     tracepoint <- c(0, 0)
#     param_index <- sample(
#         seq_len(nrow(params)),
#         n_iterations - 1,  # -1 because the first point is always (0, 0)
#         replace = TRUE,
#         prob = params[, "p"]
#     )
#     result <- data.frame(
#         x = numeric(n_iterations),
#         y = numeric(n_iterations)
#     )
#     abcd_index <- match(c("a", "b", "c", "d"), colnames(params))
#     ef_index <- match(c("e", "f"), colnames(params))
#     for (i in seq_len(n_iterations - 1)) {
#         k <- param_index[i]
#         m <- matrix(params[k, abcd_index], nrow = 2, byrow = TRUE)
#         tracepoint <- m %*% tracepoint + params[k, ef_index]
#         result[i + 1, ] <- tracepoint
#     }
#
#     if (!plot) return(result)
#     opar <- par(no.readonly = TRUE)
#     on.exit(par(opar))
#     par(mar = rep(0, 4))
#     plot(result, pch = 20, axes = TRUE, ann = FALSE, asp = 1, cex = cex, ...)
# }
#
# # chaos_fern(30000, col = "forestgreen")
# #
# # Different parameter set taken from Wikipedia:
# # alternative <- data.frame(
# #     a = c(0, 0.95, 0.035, -0.04 ),
# #     b = c(0, 0.005, -0.2, 0.2),
# #     c = c(0, -0.005, 0.16, 0.16),
# #     d = c(0.25, 0.93, 0.04, 0.04),
# #     e = c(0, -0.002, -0.09, 0.083),
# #     f = c(-0.4, 0.5, 0.02, 0.12),
# #     p = c(0.02, 0.84, 0.07, 0.07)
# # )
# # chaos_fern(20000, params = alternative, col = "forestgreen")
