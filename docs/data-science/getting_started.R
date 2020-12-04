roll <- function(faces = 1:6, num_of_dice = 1) {
  dice <- sample(x = faces, size = num_of_dice, replace = TRUE, 
                 probabilities_vector <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  sum(dice)
}

results <- replicate(n = 100, expr = roll(), simplify=TRUE)
hist(results)
