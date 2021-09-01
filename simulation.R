library(tidyverse)

funs <- dir("functions")
for (name in funs[str_detect(funs, "R$")]){
  source(paste0("functions/", name))
}


set.seed(2108201715) # beter een groot getal

n <- 20
p <- c(100, 300, 1e3, 3e3, 1e4)
sig_p <- 0
d <- 0
nsims <- 100
correlated <- "no"

outpath <- paste0("output/p_n", n, "sigp", sig_p, "d", d, "nsims", nsims, correlated, "corr")

df_detections <- tibble(p = rep(p, each = 5),
                          result = vector(length = length(p)))
df_performances <- tibble(p = rep(p, each = 5),
                          result = vector(length = length(p)))
df_nulls <- tibble(p = rep(p, each = 5),
                          result = vector(length = length(p)))
df_times <- tibble(p = rep(p, each = 5),
                   result = vector(length = length(p)))

model_summary <- c(t_fdr_summary, t_tweedie_summary, lasso_summary, boruta_summary, svm_summary)

for (i in 1:length(p)){
  results <- run_experiment(n, p[i], sig_p, d, nsims, correlated)
  for (j in 1:5) {
    df_detections$result[5 * (i-1) + j] <- list(model_summary[[j]](results)$detections)
    df_performances$result[5 * (i-1) + j] <- list(model_summary[[j]](results)$performances)
    df_nulls$result[5 * (i-1) + j] <- list(model_summary[[j]](results)$nullresults)
    df_times$result[5 * (i-1) + j] <- list(model_summary[[j]](results)$time)
  }
}

df_detections %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims, correlated) %>%
  write_csv(paste0(outpath, "_detect.csv"))

df_performances %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims, correlated) %>%
  write_csv(paste0(outpath, "_perf.csv"))

df_nulls %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims, correlated) %>%
  write_csv(paste0(outpath, "_null.csv"))

df_times %>%
  unnest(result) %>%
  add_column(n, sig_p, d, nsims, correlated) %>%
  write_csv(paste0(outpath, "_time.csv"))

oldseed <- .Random.seed
saveRDS(oldseed, paste0("output/", Sys.time(), " oldseed.RDS"))
