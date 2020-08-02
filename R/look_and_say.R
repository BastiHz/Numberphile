#' Look-and-Say Sequence
#'
#' A sequence which describes its previous term.
#'
#' @param n The number of iteration steps.
#' @param seed The starting sequence.
#' @param return_all Logical. Should all steps be returned? Defaults to
#'   \code{TRUE}.
#'
#' @references This sequence in the On-Line Encyclopedia of Integer Sequences:
#'   \href{https://oeis.org/A005150}{A005150}.
#'
#'   The Numberphile video featuring John Conway and Brady Haran:
#'   \href{https://youtu.be/ea7lJkEhytA}{Look-and-Say Numbers (feat John
#'   Conway)}.
#'
#' @return The final iteration of the sequence as a vector if \code{return_all}
#'   is \code{FALSE}, otherwise a list of all steps of the sequence.
#'
#' @examples
#' look_and_say(seed = 1, n = 7, return_all = TRUE)
#'
#' @export
look_and_say <- function(n = 1, seed = 1, return_all = FALSE) {
    stopifnot(n > 0)
    all_answers = list(seed)
    for (i in seq_len(n)) {
        x <- rle(seed)
        seed <- c(rbind(x$lengths, x$values))
        if (return_all) all_answers[[i + 1]] <- seed
    }
    if (return_all) all_answers else seed
}


# TODO: clean this script
# TODO: test the function

# # calculate conway's constant:
# foo <- look_and_say(1, n = 50, return_all = T)
# x <- numeric(length(foo) - 1)
# for (i in seq_along(foo)[-1]) {
#     x[i - 1] <- length(foo[[i]]) / length(foo[[i - 1]])
# }
# plot(x)
#
#
# # different seeds:
# look_and_say(1:3, n = 10, return_all = T)



# # Version without rle():
# seed = c(1, 2, 1, 1)
# i <- 1
# result <- numeric()
# while (i <= length(seed)) {
#     x <- seed[i]
#     count <- 1
#     while (i < length(seed) && x == seed[i + 1]) {
#         count <- count + 1
#         i <- i + 1
#     }
#     result <- c(result, count, x)
#     i <- i + 1
# }
# result
# seed <- result
