source('../../data/setup.R')

pos_metrics_df <- data.frame(read.csv('../../data/Isabelle2022/pos_metrics.csv.gz'))
#name,theory,pos,out_degree,eigenvector,in_degree,closeness,betweenness,clustering
pos_metrics_agg_df <- pos_metrics_df %>%
  group_by(theory) %>%
  summarise(out_min = min(out_degree), out_max = max(out_degree), out_avg = mean(out_degree), out_med = median(out_degree),
            eigen_min = min(eigenvector), eigen_max = max(eigenvector), eigen_avg = mean(eigenvector), eigen_med = median(eigenvector),
            in_min = min(in_degree), in_max = max(in_degree), in_avg = mean(in_degree), in_med = median(in_degree),
            close_min = min(closeness), close_max = max(closeness), close_avg = mean(closeness), close_med = median(closeness),
            between_min = min(betweenness), between_max = max(betweenness), between_avg = mean(betweenness), between_med = median(betweenness),
            cluster_min = min(clustering), cluster_max = max(clustering), cluster_avg = mean(clustering), cluster_med = median(clustering),
            .groups = 'drop')

pos_metrics_agg_df %>%
  write.csv(gzfile('../../data/Isabelle2022/pos_metrics_agg.csv.gz'), row.names = FALSE, na = '')