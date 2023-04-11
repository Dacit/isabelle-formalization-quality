source('../data/setup.R')

# Load all data
doc_sessions_df <- data.frame(read.csv('../data/Isabelle2022/doc_sessions.csv'))
example_sessions_df <- data.frame(read.csv('../data/Isabelle2022/example_sessions.csv'))
tool_sessions_df <- data.frame(read.csv('../data/Isabelle2022/tool_sessions.csv'))
example_theories_df <- data.frame(read.csv('../data/Isabelle2022/example_theories.csv'))
change_df <- data.frame(read.csv('../data/Isabelle2022/change_frequencies.csv'))
line_count_df <- data.frame(read.csv('../data/Isabelle2022/line-count.csv'))
lints_df <- data.frame(read.csv('../data/Isabelle2022/lints.csv'))
lint_freq_df <- data.frame(read.csv('../data/Isabelle2022/lint-frequency.csv'))

# Generate datasets
lints_change_df <- change_df %>%
  anti_join(doc_sessions_df, by = 'session') %>%
  anti_join(example_sessions_df, by = 'session') %>%
  anti_join(tool_sessions_df, by = 'session') %>%
  anti_join(example_theories_df, by = 'theory') %>%
  inner_join(line_count_df, by = 'theory') %>%
  inner_join(lints_df, by = 'theory') %>%
  inner_join(lint_freq_df, by = 'theory') %>%
  mutate(
    maintain_churn = churn_multiple / time_days,
    improve_churn = churn_single / time_days,
    change_churn = (multiple + single) / time_days,
    maintain_freq = multiple / time_days,
    improve_freq = single / time_days,
    change_freq = (single + multiple) / time_days)

lint_counts_df <- lints_change_df %>%
  summarise(across(where(is.numeric), sum))
num_corr_thys <- length(unique(lints_change_df$theory))

avg_changes_size <- sum(lints_change_df$churn_multiple + lints_change_df$churn_single) / sum(lints_change_df$multiple + lints_change_df$single)
avg_change_days <- mean(1 / lints_change_df$change_freq)

tikz(file = 'change_violinplot.tex', width = 4.8, height = 3, standAlone=FALSE)
lints_change_df %>%
  transmute('Multiple' = maintain_freq, 'Single' = improve_freq,
            Lints = ifelse(frequency > 0.01, 'Frequent', 'Infrequent')) %>%
  melt() %>%
  ggplot(aes(x=variable, y=value, fill=Lints)) + geom_split_violin(trim = TRUE, scale = 'count') +
    theme_pubr(legend = 'right') + xlab(NULL) + ylab('Changes per Day') + ylim(0, 0.010) +
    coord_flip()
dev.off()

mypcor <- function(third) {
  change_overall <- pcor(lints_change_df %>% transmute(frequency = change_freq, sloc = sloc, lints = { { third } }), method = 'spearman')
  change_maintain <- pcor(lints_change_df %>% transmute(frequency = maintain_freq, sloc = sloc, lints = { { third } }), method = 'spearman')
  change_improve <- pcor(lints_change_df %>% transmute(frequency = improve_freq, sloc = sloc, lints = { { third } }), method = 'spearman')
  churn_overall <- pcor(lints_change_df %>% transmute(frequency = change_churn, sloc = sloc, lints = { { third } }), method = 'spearman')
  churn_maintain <- pcor(lints_change_df %>% transmute(frequency = maintain_churn, sloc = sloc, lints = { { third } }), method = 'spearman')
  churn_improve <- pcor(lints_change_df %>% transmute(frequency = improve_churn, sloc = sloc, lints = { { third } }), method = 'spearman')

  sprintf('%s & %s & %s & & %s & %s & %s',
    spearman_val(change_overall$estimate['frequency', 'lints'], change_overall$p.value['frequency', 'lints']),
      spearman_val(change_maintain$estimate['frequency', 'lints'], change_maintain$p.value['frequency', 'lints']),
      spearman_val(change_improve$estimate['frequency', 'lints'], change_improve$p.value['frequency', 'lints']),
      spearman_val(churn_overall$estimate['frequency', 'lints'], churn_overall$p.value['frequency', 'lints']),
      spearman_val(churn_maintain$estimate['frequency', 'lints'], churn_maintain$p.value['frequency', 'lints']),
      spearman_val(churn_improve$estimate['frequency', 'lints'], churn_improve$p.value['frequency', 'lints']))
}
# Changes to multiple things

apply_isar <- mypcor(apply_isar_switch)
finders <- mypcor(counter_example_finder + proof_finder)
diagnostic <- mypcor(diagnostic_command)
global_att <- mypcor(global_attribute_changes)
unnamed <- mypcor(global_attribute_on_unnamed_lemma)
unrestricted_auto <- mypcor(unrestricted_auto)
simplifier_initial <- mypcor(simplifier_isar_initial_method)
complex_method <- mypcor(complex_method + complex_isar_initial_method)
tactic_proofs <- mypcor(tactic_proofs)

# others (that are not just technical)
bad_style <- mypcor(bad_style_command)
lemma_transform <- mypcor(lemma_transforming_attribute)
short_name <- mypcor(short_name)
implicit_rule <- mypcor(implicit_rule)
use_isar <- mypcor(use_isar)
