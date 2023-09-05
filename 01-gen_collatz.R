library(tidyverse)

gen_collatz <- function(n) {
  # Initialize a vector to store the sequence
  collatz_seq <- c()
  
  # Check for invalid input
  if (!is.numeric(n) || n <= 0 || round(n) != n) {
    cli::cli_abort("Invalid input: Please enter a positive integer.")
  }
  
  # Loop to generate the Collatz sequence
  while (n != 1) {
    # Add the current value of n to the sequence
    collatz_seq <- c(collatz_seq, n)
    
    # Apply the Collatz rules
    if (n %% 2 == 0) {  # n is even
      n <- n / 2
    } else {  # n is odd
      n <- 3 * n + 1
    }
  }
  
  # Add the final 1 to the sequence
  c(collatz_seq, 1)

}

collatz_df <-
  tibble(start = 1:10000) %>%
  mutate(seq = map(start, gen_collatz))
