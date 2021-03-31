results <- res

summary_functions <- paste0("functions/", dir("functions")[dir("functions") %>% str_detect("summary.R$")] )

sapply(summary_functions, source)

summary_functions <- c(t_fdr_summary, t_tweedie_summary, lasso_summary, boruta_summary, svm_summary)



# detections
out <- tibble(result = vector(length = 5))

for (i in 1:5) {
  out$result[i] <- list(summary_functions[[i]](res)$detections)
}


## mean number of detections
out %>%
  unnest(result) %>%
  mutate(method=ordered(method, levels=c("fdr", "tweedie", "lasso", "boruta", "svm"))) %>%
  ggplot(aes(method, mean_nr, fill=detection)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=mean_nr - 1.96*mean_se, ymax= mean_nr + 1.96*mean_se), position = position_dodge(preserve="total")) +
  labs(x="",
       y="",
       title= "Mean number of true / false detections",
       subtitle = "n=50, p=100, sig_p=3, d=1.19, nsims=10") +
  scale_fill_discrete(direction = -1)


# chance of detection
out %>%
  unnest(result) %>%
  mutate(method=ordered(method, levels=c("fdr", "tweedie", "lasso", "boruta", "svm"))) %>%
  ggplot(aes(method, a_detection, fill=detection)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=a_detection - 1.96*a_det_se, ymax= a_detection + 1.96*a_det_se), position = position_dodge(preserve="total")) +
  labs(x="",
       y="",
       title= "Chance of at least one true / false detection",
       subtitle = "n=50, p=100, sig_p=3, d=1.19, nsims=10") +
  scale_fill_discrete(direction = -1) 


# performance

out2 <- tibble(result = vector(length = 5))

for (i in 1:5) {
  out2$result[i] <- list(summary_functions[[i]](res)$performances)
}

out2 %>%
  unnest(result) %>%
  mutate(method=ordered(method, levels=c("fdr", "tweedie", "lasso", "boruta", "svm"))) %>% 
  ggplot(aes(method, test_auc)) +
  geom_point() +
  geom_errorbar(aes(ymin=test_auc - 1.96*test_auc_se, ymax=test_auc + 1.96*test_auc_se), width=0.2) +
  geom_point(aes(method, pop_auc), shape=17, size=2, color="red") +
  geom_point(aes(method, data_auc), shape=15, size = 2, color = "blue") +
  geom_vline(xintercept = 2.5, linetype="dotted") +
  labs(x="",
       y="",
       title= "Estimated AUC",
       subtitle = "n=50, p=100, sig_p=3, d=1.19, nsims=10")
