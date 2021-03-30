# summary of results from t-tests with fdr correction


t_fdr_summary <- function(results_of_experiment){
  results_of_experiment <- results_of_experiment$t_fdr
  nsims <- nrow(results_of_experiment)
  
  true_false_detections <- results_of_experiment %>%
    mutate(i=factor(i)) %>%
    rowwise() %>%
    mutate(detections = ifelse(is.na(perf_test), NA, list(detections))) %>%
    unnest(detections) %>%
    rowwise() %>%
    mutate(detection = factor(detections %in% sig_features,
                              levels = c("TRUE", "FALSE"))) %>%
    group_by(i) %>%
    count(detection) %>%
    ungroup() %>%
    complete(i, detection, fill = list(n=0)) 
  
  detection_summary <- true_false_detections %>%
    group_by(detection) %>% # for TRUE and FALSE detections:
    summarise(mean_nr = mean(n),           # mean number of detections
              mean_se = sd(n)/sqrt(nsims), # MCSE
              a_detection = mean(n>0),     # proportion of simulations that result in 'a detection' 
              a_det_se = sqrt(a_detection * (1-a_detection)/nsims)) # MC SE of proportion
  
  performance_summary <- results_of_experiment %>%
    summarise(test_auc = mean(perf_test, na.rm=T), # calculates the mean auc (test, pop_l, pop_f)
              test_auc_se = sd(perf_test, na.rm = T)/sqrt(nsims-sum(is.na(perf_test))),
              data_auc = mean(perf_pop),
              auc_bias = test_auc - data_auc,
              auc_bias_se = sd(perf_test - perf_pop, na.rm = T)/sqrt(nsims-sum(is.na(perf_test))),
              auc_sd = sd(perf_test, na.rm = T),
              auc_sd_se = sd(perf_test, na.rm = T)/sqrt(2*(nsims-sum(is.na(perf_test))-1))
              ) 
  
  # null results
  nulls <- results_of_experiment %>%
    mutate(i=factor(i)) %>%
    unnest(detections) %>%
    complete(i) %>%
    group_by(i) %>%
    summarise(nullresult=any(is.na(detections))) %>%
    count(nullresult) %>%
    mutate(proportion = n/sum(n)) %>%
    filter(nullresult)
  
  
  list(raw_results = results_of_experiment,
       counts = true_false_detections,
       detections = detection_summary,
       performances = performance_summary,
       nullresults = nulls)
}
