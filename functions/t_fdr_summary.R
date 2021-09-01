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
    # find true detections and convert to factor, this is to be able to ....
    mutate(detection = factor(ifelse(is.na(detections), NA, detections %in% sig_features), levels = c("TRUE", "FALSE"))) %>%
    group_by(i) %>%                                           # .... 'complete' later on. Otherwise this can be a hindrance
    count(detection) %>%                                      # when no TRUE or FALSE detections are made in the entire run
    ungroup() %>%
    complete(i, detection, fill = list(n=0)) %>%
    drop_na()
  
  detection_summary <- true_false_detections %>%
    group_by(detection) %>% # for TRUE and FALSE detections:
    summarise(mean_nr = mean(n),           # mean number of detections
              mean_se = sd(n)/sqrt(nsims), # MCSE
              a_detection = mean(n>0),     # proportion of simulations that result in 'a detection' 
              a_det_se = sqrt(a_detection * (1-a_detection)/nsims)) %>% # MC SE of proportion
    add_column(method = "fdr")
    
  performance_summary <- results_of_experiment %>%
    summarise(test_auc = mean(perf_test, na.rm=T), # calculates the mean auc (test, pop_l, pop_f)
              test_auc_se = sd(perf_test, na.rm = T)/sqrt(nsims-sum(is.na(perf_test))),
              data_auc = mean(perf_pop),
              auc_bias = test_auc - data_auc,
              auc_bias_se = sd(perf_test - perf_pop, na.rm = T)/sqrt(nsims-sum(is.na(perf_test))),
              auc_sd = sd(perf_test, na.rm = T),
              auc_sd_se = sd(perf_test, na.rm = T)/sqrt(2*(nsims-sum(is.na(perf_test))-1))
              ) %>% 
    add_column(method = "fdr")
  
  # null results
  nulls <- results_of_experiment %>%
    mutate(i=factor(i)) %>%
    unnest(detections) %>%
    complete(i) %>%
    group_by(i) %>%
    summarise(nullresult=factor(any(is.na(detections)), levels= c("TRUE", "FALSE"))) %>%
    count(nullresult) %>%
    complete(nullresult, fill = list(n=0)) %>%
    mutate(p = n/sum(n),
           p_se = sqrt(p * (1-p)/nsims)) %>% 
    add_column(method = "fdr") %>%
    rename(prop = p,
           number = n,
           prop_se = p_se) %>%
    filter(nullresult == "TRUE")
  
  # times
  time <- results_of_experiment %>%
    summarise(mean_time = mean(time),
              mean_time_se = sd(time)/sqrt(nsims)) %>%
    add_column(method = "fdr")
  
  list(raw_results = results_of_experiment,
       counts = true_false_detections,
       detections = detection_summary,
       performances = performance_summary,
       nullresults = nulls,
       time = time)
}
