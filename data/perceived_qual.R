source('../data/setup.R')

# Load all data
lint_freq_df <- data.frame(read.csv('../data/Isabelle2022/lint-frequency.csv'))
manual_df <- data.frame(read.csv('../data/Isabelle2022/manual_quality.csv'))
llama_df <- data.frame(read.csv('../data/Isabelle2022/theory_chunks/theory_quality_pred.csv'))

# Generate datasets
manual_compare_df <- manual_df %>%
  inner_join(lint_freq_df, by = 'theory') %>%
  inner_join(llama_df, by = 'theory')

llama_filtered_df <- llama_df %>%
  inner_join(lint_freq_df, by = 'theory')

# Compare med/mean of bad-eggs
lint_overall_med <- median(lint_freq_df$frequency)
lint_overall_mean <- mean(lint_freq_df$frequency)
lint_bad_med <- median((manual_compare_df %>% filter(bad == 1))$frequency)
lint_bad_mean <- mean((manual_compare_df %>% filter(bad == 1))$frequency)

llama_overall_med <- median(llama_filtered_df$score)
llama_overall_mean <- mean(llama_filtered_df$score)
llama_bad_med <- median(manual_compare_df$score)
llama_bad_mean <- mean(manual_compare_df$score)