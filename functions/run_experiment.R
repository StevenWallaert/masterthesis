# function to run experiment 
# currently only supporting logistic lasso
#
#
# nsims - number of repetitions

source("functions/create_data.R")


library(glmnet)
library(ROCR)
library(doParallel)
library(Boruta)
library(randomForest)
library(tidyverse)
source("SVM-RFE-master/msvmRFE.R")
library(e1071)
ncores <- 5
prop_train <- 2/3
pop_size <- 1e4


run_experiment <- function(n, p, sig_p, d, nsims){
  suppressMessages(suppressWarnings({

    # parameters
    # prop_train: proportion train - test
    # pop_size: population size

    # dataframes to store results
    # # lasso
    #create an empty dataframe to store the results of current condition
    results <- tibble(i = 1:nsims, # number of repetition
                      detections = vector(length = nsims), # coefficients dataframe
                      sig_features = vector(length = nsims), # the significant features
                      p = rep(p, nsims),  # number of features
                      measure_cv = vector(length = nsims), # which measure was used: auc or something else?
                      perf_test = vector(length = nsims), # performance result on test set
                      perf_pop = vector(length = nsims), # performance result on population set
                      #perf_pop_full = vector(length = nsims) # performance model built on full dataset on population set
    )
    # # t-test FDR control
    ttest_fdr <- tibble(i = 1:nsims, # number of repetition
                        detections = vector(length = nsims), # detected features i.e. q-val < .05
                        sig_features = rep(list(1:sig_p), nsims), # the significant features
                        p = rep(p, nsims),  # number of features
                        perf_test = vector(length = nsims), # estimate of AUC
                        perf_pop = case_when(d == 0.36 ~ 0.6,
                                             d == 0.74 ~ 0.7,
                                             d == 1.19 ~ 0.8,
                                             d == 1.81 ~ 0.9)
    )
    # # t-test Tweedie
    ttest_tweedie <- tibble(i = 1:nsims, # number of repetition
                        detections = vector(length = nsims), # detected features i.e. q-val < .05
                        sig_features = rep(list(1:sig_p), nsims), # the significant features
                        p = rep(p, nsims),  # number of features
                        perf_test = vector(length = nsims), # estimate of AUC
                        perf_pop = case_when(d == 0.36 ~ 0.6,
                                             d == 0.74 ~ 0.7,
                                             d == 1.19 ~ 0.8,
                                             d == 1.81 ~ 0.9)
    )
    # # Boruta
    boruta <- tibble(i = 1:nsims, # number of repetition
                     detections = vector(length = nsims), # detected features i.e. q-val < .05
                     sig_features = rep(list(1:sig_p), nsims), # the significant features
                     p = rep(p, nsims),  # number of features
                     perf_test = vector(length = nsims), # estimate of AUC
                     perf_pop = vector(length = nsims)
                     )
    # # SVM
    svm_results <- tibble(i = 1:nsims, # number of repetition
                          detections = vector(length = nsims), # detected features i.e. q-val < .05
                          sig_features = rep(list(1:sig_p), nsims), # the significant features
                          p = rep(p, nsims),  # number of features
                          perf_test = vector(length = nsims), # estimate of AUC
                          perf_pop = vector(length = nsims)
    )
    # create a population data set to test the models on
    pop <- create_data(pop_size, p, sig_p, d)
    
    # nsims repetitions
    pb <- txtProgressBar(min = 0, max = nsims, style = 3)
    for (i in seq_len(nsims)){
      
      #### Data setup ####
      # create simulation data set
      #simdat <- create_data(n, p, sig_p, d)
      
      # train - test split, stratified
      # X_train <- simdat$XY %>%
      #   group_by(Y) %>%
      #   slice_sample(prop = prop_train) %>% 
      #   ungroup()
      
      # separate Y from X
      # Y_train <- X_train %>% select(Y) %>% as.matrix()
      
      # delete Y out of dataframe, don't convert to matrix yet, will need it (cf anti_join)
      # X_train <- X_train %>% select(!Y)
      
      # create test set, use all observations not in train
      # X_test <- simdat$XY %>%
      #   anti_join(X_train, by = "...1")
      # 
      # # now convert X_train to matrix
      # X_train <- X_train %>% as.matrix()
      # 
      # # separate Y from X in test
      # Y_test <- X_test %>% select(Y) %>% as.matrix()
      # 
      # # delete Y out of dataframe and convert to matrix
      # X_test <- X_test %>% select(!Y) %>% as.matrix()
      
      ### better data setup ###
      # profiling pointed out that the previous was slow
      simdat <- create_data(n, p, sig_p, d)
      
      # stratified sampling -> train - test
      train_ind <-
        tibble(Y=simdat$Y) %>%
        rownames_to_column() %>%
        group_by(Y) %>%
        slice_sample(prop = prop_train) %>%
        pull(rowname) %>%
        as.numeric()
      
      Y_train <- simdat$Y[train_ind]
      
      X_train <- simdat$X[train_ind,]
      
      X_test <- simdat$X[-train_ind,]
      
      Y_test <- simdat$Y[-train_ind]
      
######## logistic lasso ####
      # step 1: use 5-fold CV to obtain optimal value for lambda
      
      cl <- makeCluster(ncores) # added parallel support because of long computation times
      registerDoParallel(cl)
      
      # parallel only profitable when p > 1000 (after experimenting)
      if(p > 1000) {
        para <- TRUE
      } else {
        para <- FALSE
      }
      
      ## the while loop and try construction is there to catch errors in case of extremely small sample sizes
      ## in those cases it happens that some folds have 0 observations in one of the 2 classes, result: error
      cvfit <- 1 # just a variable so that the program can enter the while loop
      
      while(class(cvfit) != "cv.glmnet"){
        # catches the error in case of an error, or the result in case of a successful try
        cvfit <- try({
          cv.glmnet(X_train, Y_train, family = "binomial", alpha = 1, nfolds = 5, type.measure = "auc", parallel = para)
        }, silent = TRUE) 
      }
      stopCluster(cl)
      # step 2: fit model using optimal value for lambda obtained in step 1
      fit <- glmnet(X_train, Y_train, family = "binomial", alpha = 1, lambda = cvfit$lambda.min)
      
      #### Performance ####
      # step 3: performance on test set
      pred <- prediction(predict(fit, newx = X_test, type = "response"), Y_test)
      results$perf_test[i] <- performance(pred, "auc")@y.values[[1]]
      
      # step 4: performance on population set, trained only on training set, hence limited
      pred_pop <- prediction(predict(fit, newx = pop$X, type = "response"), pop$Y)
      results$perf_pop[i] <- performance(pred_pop, "auc")@y.values[[1]]
      
      # step 5: performance on population set, trained on full set, hence full
      # fullfit <- glmnet(simdat$X, simdat$Y, family = "binomial", alpha = 1, lambda = cvfit$lambda.min)
      # pred_pop_full <- prediction(predict(fullfit, newx = pop$X, type = "response"), pop$Y)
      # results$perf_pop_full[i] <- performance(pred_pop_full, "auc")@y.values[[1]]
      
      #### Save results ####
      # extract coefficients and store in results dataframe
      # Question: using full fit or not?
      fit_coef <- coef(fit)
      
      results$detections[i] <- list(names(fit_coef[fit_coef[,1] != 0, ])) # extract names nonzero features
      
      
      # for reference, the significant features
      results$sig_features[i] <- list(sapply(simdat$sig_features, function(x) paste0("V", x)))
      
      
      # although "auc" is chosen as type.measure in cv.glmnet, this is not always possible
      # it may be good to keep track of this as well
      results$measure_cv[i] <- cvfit$name
      
######## ttest fdr ####
      # step 1: perform t-tests and extract t-stat and pval
      out <- map_df(1:p, function(j){
        test <- t.test(simdat$X[,j]~simdat$Y)
        list(statistic = test$statistic,
             pval = test$p.value,
             df = test$parameter)
      })
      
      # step 2: FDR correction
      out$qval <- p.adjust(out$pval, method = "BH")
      
      # step 3: keep only the significant features
      ttest_fdr$detections[i] <- list(which(out$qval < 0.05)) # store detections
      
      # step 4: calculate the AUC
      if(ttest_fdr$detections[i] %>% unlist() %>% is_empty()) {
        ttest_fdr$perf_test[i] <- NA  # not if there are no significant features
      } else {
        ttest_fdr$perf_test[i] <- mean(map_dbl(ttest_fdr$detections[i], function(j) { # looks for estimated AUC per detection
        max(c(mean(simdat$X[simdat$Y==1, j] > simdat$X[simdat$Y==0, j]), # and then takes the average
              mean(simdat$X[simdat$Y==1, j] < simdat$X[simdat$Y==0, j])))
      }))
      }
      
##### Tweedie ####
      # step 1: make counts per bin
      counts <- cut(out$statistic, 30) %>%
        as_tibble_col("bins") %>%
        count(bins) %>%
        mutate(t = map_dbl(str_extract_all(bins, "-?\\d+\\.?\\d+"), function(x) mean(as.numeric(x)) ) )
      
      # step 2: fit glm
      fit <- glm(n ~ poly(t, 5, raw = T), family = poisson(link = "log"), data = counts) 
      betas <- coef(fit)
      
      # step 3: calculate l'()
      l_prime <- function(x){
        x <- c(1, poly(x, degree = 4, raw=T)) 
        
        as.vector(x %*% (betas[2:length(betas)] * 1:5 ))
      }
      
      var_t <- var(out$statistic)
      
      # step 4: calculate corrected test statistics and corresponding p-values, assuming t is *approx* (cf. Efron for this) normally distributed
      out <- out %>%
        rowwise() %>%
        mutate(corrected_t = statistic + var_t*l_prime(statistic),
               corrected_p = 2*(1-pt(abs(corrected_t), df)))
      
      ttest_tweedie$detections[i] <- list(which(out$corrected_p < 0.05)) # store detections
      
      # step 5: calculate the AUC
      if(ttest_tweedie$detections[i] %>% unlist() %>% is_empty()) {
        ttest_tweedie$perf_test[i] <- NA  # not if there are no significant features
      } else {
        ttest_tweedie$perf_test[i] <- mean(map_dbl(ttest_tweedie$detections[i], function(j) { # looks for estimated AUC per detection
          max(c(mean(simdat$X[simdat$Y==1, j] > simdat$X[simdat$Y==0, j]), # and then takes the average
                mean(simdat$X[simdat$Y==1, j] < simdat$X[simdat$Y==0, j])))
        }))
      }
      
##### Boruta #####
      res_boruta <- Boruta(x=simdat$X,
                           y=simdat$Y,
                           num.trees = 2000, pValue = 0.05)

      boruta$detections[i] <- list(which(res_boruta$finalDecision == "Confirmed"))
      
      if (length(boruta$detections[i] %>% unlist())==0) {
        boruta$perf_test[i] <- NA
        boruta$perf_pop[i] <- NA
      } else {
        rf_mod <- randomForest(X_train[,unlist(boruta$detections[i])], factor(Y_train), ntree = 2000)
        prediction_rf <- prediction(as.numeric(predict(rf_mod, newdata = X_test[,unlist(boruta$detections[i])])), labels = Y_test)
        boruta$perf_test[i] <- performance(prediction_rf, "auc")@y.values[[1]]
        
        prediction_rf <- prediction(as.numeric(predict(rf_mod, newdata = pop$X[,unlist(boruta$detections[i])])), labels = pop$Y)
        boruta$perf_pop[i] <- performance(prediction_rf, "auc")@y.values[[1]]
      }
      

##### RF RFE ####
       #res_rf <- varSelRF::varSelRF(X_train, factor(Y_train), whole.range = T, vars.drop.frac = 0.5)

      
##### SVM RFE ####
      # source("SVM-RFE-master/msvmRFE.R")
      svm_rfe <- svmRFE(cbind(Y_train, X_train), k = 5, halve.above = 250)
      
      n_feat <- max(2, p %/% 100) # minimum 2 variables, and at most 1% cf. ....
      
      svm_results$detections[i] <- list(svm_rfe[1:n_feat])
      
      svm_mod <- svm(X_train[, unlist(svm_results$detections[i])], factor(Y_train), kernel="linear", cost=10)
      prediction_svm <- prediction(as.numeric(predict(svm_mod, newdata = X_test[,unlist(svm_results$detections[i])])), labels = factor(Y_test))
      svm_results$perf_test[i] <- performance(prediction_svm, "auc")@y.values[[1]]
      
      svm_mod <- svm(X_train[, unlist(svm_results$detections[i])], factor(Y_train), kernel="linear", cost=10)
      prediction_svm <- prediction(as.numeric(predict(svm_mod, newdata = pop$X[,unlist(svm_results$detections[i])])), labels = factor(pop$Y))
      svm_results$perf_pop[i] <- performance(prediction_svm, "auc")@y.values[[1]]
        
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }))
  list(lasso = results, 
       t_fdr = ttest_fdr,
       t_tweedie= ttest_tweedie,
       boruta = boruta,
       svm=svm_results)
}

