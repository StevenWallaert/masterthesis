library(tidyverse)

source("functions/create_data.R")
source("functions/run_experiment.R")
source("functions/detections_summary.R")

set.seed(1)
n <- 10
p <- c(10, 30, 100, 300, 1e3, 3e3, 6e3)
sig_p <- 1
d <- 0.36
nsims <- 400

df_detections <- tibble(p = p,
                        result = vector(length = length(p)))
df_performances <- tibble(p = p,
                          result = vector(length = length(p)))

for (i in 1:length(p)){
  results <- run_experiment(n, df_detections$p[i], sig_p, d, nsims)
  results <- detections_summary(results)
  df_detections$result[i] <- list(results$detections)
  df_performances$result[i] <- list(results$performances)
}

df_detections %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims) %>%
  write_csv("output/p_n10_sigp1_d036_nsims400_detect.csv")

df_performances %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims) %>%
  write_csv("output/p_n10_sigp1_d036_nsims400_perf.csv")