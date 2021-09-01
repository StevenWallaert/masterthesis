# process outputs in the case of LASSO


lasso_summary <- function(results_of_experiment){
  results_of_experiment <- results_of_experiment$lasso
  nsims <- nrow(results_of_experiment)
  
  # count True and False detections
  #
  # Result = table with count of TRUE and FALSE detections per simulation (i)
  #
  true_false_detections <- results_of_experiment %>%
    rowwise() %>%
    mutate(detections = ifelse(is.null(detections), list(NA), list(detections))) %>% # fill empty detections with NA, this allows to complete later on
    unnest(detections) %>%                                                           # as in TRUE: 0 FALSE: 0
    rowwise() %>%
   # find true detections and convert to factor, this is to be able to ....
    mutate(detection = factor(ifelse(is.na(detections), NA, detections %in% sig_features), levels = c("TRUE", "FALSE"))) %>%
    group_by(i) %>%                                           # .... 'complete' later on. Otherwise this can be a hindrance
    count(detection) %>%                                      # when no TRUE or FALSE detections are made in the entire run
    ungroup() %>%
    mutate(n = ifelse(detection=="TRUE", n, n-1)) %>% # subtract intercept from FALSE detections, TRUEs are left as is
    complete(i, detection, fill = list(n = 0)) %>%  # add explicit 0-counts where no detections are made
    drop_na()

  
  # barplot - not used later on so it is commented out for now
  # barplot_detections <- true_false_detections %>%
  #   ggplot(aes(x=n, y = ..prop.., fill = detection)) +
  #   geom_bar(position = "dodge", stat = "count") +
  #   facet_grid(detection~.)
  
  # detection summary
  #
  # Result = table showing the mean nr of TRUE/FALSE detections + MCSE
  #                   and the proportion of sims that have a TRUE/FALSE detection + MCSE
  #
  detection_summary <- true_false_detections %>%
    group_by(detection) %>% # for TRUE and FALSE detections:
    summarise(mean_nr = mean(n),           # mean number of detections
              mean_se = sd(n)/sqrt(nsims), # MCSE
              a_detection = mean(n>0),     # proportion of simulations that result in 'a detection' 
              a_det_se = sqrt(a_detection * (1-a_detection)/nsims)) %>%#  MC SE of proportion
    add_column(method = "lasso")
  
  # performance summary
  #
  # Result = table showing mean test auc together with 'true auc' + MCSE
  #          also the sd (+MCSE) of the test auc is caluclated ~ indicates the variance of the estimator
  #          bias is derived (+MCSE)
  #          population auc/bias is derived for models built on training data (lim, l) or full data (full, f)
  performance_summary <- results_of_experiment %>%
    summarise(test_auc = mean(perf_test), # calculates the mean auc (test, pop_l, pop_f)
              test_auc_se = sd(perf_test)/sqrt(nsims),
              pop_auc = mean(perf_pop),
              pop_auc_se = sd(perf_pop)/sqrt(nsims),
              #pop_full_se = sd(perf_pop_full)/sqrt(nsims),
              auc_sd = sd(perf_test),                        # to have an indication of the variance of the estimator, sd for ease of interpretation
              auc_sd_se = sd(perf_test)/sqrt(2*(nsims-1)),         # MCSE of SD
              auc_bias = mean(perf_test - perf_pop),
              auc_bias_se = sd(perf_test - perf_pop)/sqrt(nsims),
              #bias_f = mean(perf_test - perf_pop_full),
              #bias_f_se = sd(perf_test - perf_pop_full)/sqrt(nsims)
              ) %>%
    add_column(method = "lasso")
  
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
    add_column(method = "lasso") %>%
    rename(prop = p,
           number = n,
           prop_se = p_se) %>%
    filter(nullresult == "TRUE")
    
  
  # times
  time <- results_of_experiment %>%
    summarise(mean_time = mean(time),
              mean_time_se = sd(time)/sqrt(nsims)) %>%
    add_column(method = "lasso")
  
  list(raw_results = results_of_experiment,
       counts = true_false_detections,
       detections = detection_summary,
       performances = performance_summary,
       nullresults = nulls,
       time = time)
}


