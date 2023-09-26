plot_df <-
  collatz_df %>%
  mutate(colour_me = start %in% top10longest)
ggplot(filter(plot_df, colour_me), aes(start, length)) +
  geom_point(data = plot_df, size = 1) +
  geom_point(col = "red3") +
  ggrepel::geom_text_repel(aes(label = start), col = "red3") +
  labs(x = "Starting integer", y = "Length of sequence")

plot_df <-
  collatz_df %>%
  slice_max(max_val, n = 10)
ggplot(plot_df, aes(start, max_val)) +
  geom_point(data = collatz_df, alpha = 0.5) +
  geom_point(col = "red3") +
  ggrepel::geom_text_repel(aes(label = start), col = "red3") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Starting integer", y = "Maximum sequence value")

ggplot(collatz_df, aes(y = length, fill = parity)) +
  geom_boxplot() + 
  # geom_jitter(size = 0.5, width = 0.3, alpha = 0.5) +
  facet_grid(. ~ parity) +
  labs(y = "Sequence length", x = NULL) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


