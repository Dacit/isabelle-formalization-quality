source('../../data/setup.R')

doc_sessions_df <- data.frame(read.csv('../../data/Isabelle2022/doc_sessions.csv'))
example_sessions_df <- data.frame(read.csv('../../data/Isabelle2022/example_sessions.csv'))
tool_sessions_df <- data.frame(read.csv('../../data/Isabelle2022/tool_sessions.csv'))
example_theories_df <- data.frame(read.csv('../../data/Isabelle2022/example_theories.csv'))
line_count_df <- data.frame(read.csv('../../data/Isabelle2022/line-count.csv'))
pos_metrics_agg_df <- data.frame(read.csv('../../data/Isabelle2022/pos_metrics_agg.csv.gz'))
lints_df <- data.frame(read.csv('../../data/Isabelle2022/lints.csv'))

lint_count_df <- lints_df %>%
  transmute(
    theory = theory,
    lints = apply_isar_switch +
      #bad_style_command + # apply_end was ok historically, only for new entries unacceptable
      complex_isar_initial_method +
      complex_method +
      global_attribute_changes +
      global_attribute_on_unnamed_lemma +
      low_level_apply_chain +
      simplifier_isar_initial_method +
      tactic_proofs +
      unrestricted_auto +
      counter_example_finder +
      diagnostic_command +
      proof_finder)

data_df <- line_count_df %>%
  anti_join(doc_sessions_df, by = 'session') %>%
  anti_join(example_sessions_df, by = 'session') %>%
  anti_join(tool_sessions_df, by = 'session') %>%
  anti_join(example_theories_df, by = 'theory') %>%
  inner_join(lint_count_df, by = 'theory') %>%
  inner_join(pos_metrics_agg_df, by = 'theory') %>%
  filter(sloc > 0) %>%
  mutate(frequency = lints / sloc) %>%
  select(-lints, -session)

data_df %>%
  select(theory, frequency) %>%
  write.csv('../../data/Isabelle2022/lint-frequency.csv', row.names = FALSE, na = '0')

data_df %>%
  write.csv(gzfile('../../data/Isabelle2022/graph_features.csv.gz'), row.names = FALSE, na = '0')