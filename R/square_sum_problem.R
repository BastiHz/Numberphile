# TODO: Optimization idea: start with numbers that only have one neighbor
# because they must be at the end of the chain.

# If I in the future decide to implement a way to return all possible chains
# then I have to remember that the number of valid chains must be even because
# every chain can be forwards and backwads. And remember that when a chain is
# found for a certain starting number to also look for other chains with that
# starting number.


#' Square Sum Problem
#'
#' Using the integers from 1 to n construct a chain such that each pair of
#' neighboring numbers adds up to a square number.
#'
#' The function returns the first valid chain that is found. Maybe some day I
#' will write a version that returns all possible chains.
#'
#' @param n The length of the chain.
#'
#' @references The Numberphile video featuring Matt Parker and Brady Haran:
#'   \href{https://youtu.be/G1m7goLCJDY}{The Square-Sum Problem}.
#'
#' @examples
#' square_sum_problem(15)
#'
#' @export
square_sum_problem <- function(n) {
    # This function uses backtracking to build the path.

    # Find all relevant square numbers. This is so that they only have to
    # be calculated once:
    squares <- seq_len(floor(sqrt(n + n - 1)))[-1] ^ 2

    # Find all pairwise combinations:
    pairwise_combinations <- list()
    end_numbers <- 0  # numbers with only one possible neighbor
    for (i in 1:n) {
        other_numbers <- seq(1, n)[-i]
        pairwise_sums <- i + other_numbers
        neighbors_for_i <- other_numbers[pairwise_sums %in% squares]
        if (length(neighbors_for_i) == 1) {
            end_numbers <- end_numbers + 1
            if (end_numbers > 2) {
                stop("No solution possible.")
            }
        } else if (length(neighbors_for_i) == 0) {
            stop("No neighbors for the number ", i, " found.")
        }
        pairwise_combinations[[i]] <- neighbors_for_i
    }

    # Go through the list and build the chain:
    for (i in 1:n) {
        current_chain <- i
        current_num <- i
        dead_end <- 0
        success <- TRUE
        chain_length <- length(current_chain)

        while (chain_length < n) {
            j <- !(pairwise_combinations[[current_num]] %in% current_chain) &
                pairwise_combinations[[current_num]] > dead_end
            if (any(j)) {
                dead_end <- 0
                next_num <- pairwise_combinations[[current_num]][j][1]
                current_chain <- c(current_chain, next_num)
                chain_length <- chain_length + 1
                current_num <- next_num
            } else {
                # go back
                dead_end <- current_num
                current_chain <- current_chain[-chain_length]
                chain_length <- chain_length - 1
                if (chain_length == 0) {
                    success <- FALSE
                    break
                }
                current_num <- current_chain[chain_length]
            }
        }

        if (success) return(current_chain)
    }
    stop("No valid chains found.")
}
