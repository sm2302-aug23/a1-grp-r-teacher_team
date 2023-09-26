find_backtracking <- function(sequence, start) {
  went_below_start <- FALSE
  backtracks <- 0
  max_after_backtrack <- 0
  
  for (n in sequence) {
    if (n < start) {
      went_below_start <- TRUE
    }
    if (went_below_start && n > start) {
      backtracks <- backtracks + 1
      max_after_backtrack <- max(max_after_backtrack, n)
    }
  }
  
  list(backtracks = backtracks, 
       max_after_backtrack = max_after_backtrack)
}

collatz_df <-
  collatz_df %>%
  mutate(backtrack_info = map2(seq, start, find_backtracking)) %>%
  unnest_wider(backtrack_info)

# Identify which starting integers from 1 to 10,000 exhibit backtracking in their sequences.
backtracks_df <-
  collatz_df %>%
  filter(backtracks > 0)

# For sequences that backtrack, how many times do they typically go above their starting integer?
mode_backtrack <-
  backtracks_df %>%
  group_by(backtracks) %>%
  summarise(freq = n()) %>%
  slice_max(freq) %>%
  pull(backtracks)
  
# What is the maximum value reached after the first backtrack for these sequences?
max_after_backtrack <-
  backtracks_df %>%
  pull(max_after_backtrack)

# Are backtracking sequences more common among even or odd starting integers?

even_odd_backtrack <-
  backtracks_df %>%
  group_by(parity) %>%
  summarise(freq = n()) %>%
  pull(freq)
