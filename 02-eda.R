collatz_df <-
  collatz_df %>%
  mutate(length = map_dbl(seq, length),
         parity = ifelse(start %% 2 == 0, "Even", "Odd"),
         max_val = map_dbl(seq, max)) 

# Top 10 starting integers that produce the longest sequence
top10longest <-
  collatz_df %>%
  slice_max(length, n = 10) %>% 
  pull(start)
  
# starting integers produce sequences that reach the highest maximum values.
max_val_int <-
  collatz_df %>%
  slice_max(max_val) %>%
  pull(start)

# the average length of the sequence for even starting integers compared to odd ones
parity_collatz_df <-
  collatz_df %>%
  group_by(parity) %>%
  summarise(mean_len = mean(length),
            sd_len = sd(length))
even_odd_avg_len <- parity_collatz_df$mean_len
even_odd_sd_len <- parity_collatz_df$sd_len
