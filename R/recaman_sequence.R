#' Recaman Sequence
#'
#' Always go back. If you can't go back, go forward.
#'
#' @param n The number of terms of the sequence to generate.
#' @param plot Logical. Should the sequence be plotted with semicircles?
#'
#' @return The Recamán sequence.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A005132}{A005132}.
#'
#'   The Numberphile video featuring Alex Bellos and Brady Haran:
#'   \href{https://youtu.be/FGC5TdIiT9U}{The Slightly Spooky Recaman Sequence}.
#'
#' @examples
#' r <- recaman(100, TRUE)
#'
#' @export
recaman <- function(n, plot = FALSE) {
    stopifnot(n > 0)
    if (n == 1) return(0)
    recaman_seq <- integer(n)
    for (i in 1:(n-1)) {
        last <- recaman_seq[i]
        difference <- last - i
        if (difference <= 0 || difference %in% recaman_seq) {
            recaman_seq[i + 1] <- last + i
        } else {
            recaman_seq[i + 1] <- difference
        }
    }
    if (plot) {
        plot_recaman_semicircles(recaman_seq)
        invisible(recaman_seq)
    } else {
        recaman_seq
    }
}


plot_semicircle <- function(x, y, radius, n_approx, side, ...) {
    # Draw a semicircle into an existing plot. The semicircle begins and
    # ends at the horizontal line.
    # x, y: Circle center.
    # radius: Circle radius.
    # n_approx: Number of points to approximate the circle.
    # side: Either 1 or -1. Draw above horizontal if positive
    #     and below if negative.
    # ...: Further arguments to be passed to lines().
    angle <- seq(0, pi, length.out = n_approx)
    lines(
        x + radius * cos(angle) * side,
        y + radius * sin(angle) * side,
        ...
    )
}


plot_recaman_semicircles <- function(recaman_seq) {
    # TODO: Adjust number of circle points because when there are big circles
    # then the small circles don't need to be drawn with 100 points. It could
    # depend on the max(recaman_seq) and/or max(radii).
    radii <- abs(diff(recaman_seq)) / 2
    n_circle_points <- round(radii * pi * 5)
    centers <- (head(recaman_seq, -1) + recaman_seq[-1]) / 2
    side <- rep(c(-1, 1), length.out = length(recaman_seq) - 1)

    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mar = rep(0, 4))
    max_radii <- max(radii)
    plot(
        NA,
        NA,
        xlim = c(0, max(recaman_seq)),
        ylim = c(-max_radii, max_radii),
        asp = 1,
        bty = "n",
        ann = FALSE,
        axes = FALSE
    )
    for (i in seq_along(radii)) {
        plot_semicircle(centers[i], 0, radii[i], n_circle_points[i], side[i])
    }
}
