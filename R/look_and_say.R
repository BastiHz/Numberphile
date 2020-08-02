# look-and-say sequence: https://www.youtube.com/watch?v=ea7lJkEhytA
# https://en.wikipedia.org/wiki/Look-and-say_sequence

look_and_say <- function(seed = 1, n = 1, return_all = FALSE) {
    all_answers = list(seed)
    for (i in seq_len(n)) {
        x <- rle(seed)
        seed <- c(rbind(x$lengths, x$values))
        if (return_all) all_answers[[i + 1]] <- seed
    }
    if (return_all) {
        all_answers
    } else {
        seed
    }
}


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
