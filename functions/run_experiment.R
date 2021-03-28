# function to run experiment 
# currently only supporting logistic lasso
#
#
# nsims - number of repetitions

source("functions/create_data.R")

run_experiment <- function(n, p, sig_p, d, nsims){
  suppressMessages(suppressWarnings({
    require(glmnet)
    require(ROCR)
    require(doParallel)
    ncores <- detectCores() - 1
    # parameters
    # prop_train: proportion train - test
    # pop_size: population size
    prop_train <- 2/3
    pop_size <- 1e4
    
    #create an empty dataframe to store the results of current condition
    results <- tibble(i = vector(length = nsims), # number of repetition
                      detections = vector(length = nsims), # coefficients dataframe
                      sig_features = vector(length = nsims), # the significant features
                      p = vector(length = nsims),  # number of features
                      measure_cv = vector(length = nsims), # which measure was used: auc or something else?
                      perf_test = vector(length = nsims), # performance result on test set
                      perf_pop_lim = vector(length = nsims), # performance result on population set
                      perf_pop_full = vector(length = nsims) # performance model built on full dataset on population set
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
      
      #### logistic lasso ####
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
      cvfit <- 1 # just a random variable so that the program can enter the while loop
      
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
      results$perf_pop_lim[i] <- performance(pred_pop, "auc")@y.values[[1]]
      
      # step 5: performance on population set, trained on full set, hence full
      fullfit <- glmnet(simdat$X, simdat$Y, family = "binomial", alpha = 1, lambda = cvfit$lambda.min)
      pred_pop_full <- prediction(predict(fullfit, newx = pop$X, type = "response"), pop$Y)
      results$perf_pop_full[i] <- performance(pred_pop_full, "auc")@y.values[[1]]
      
      #### Save results ####
      # extract coefficients and store in results dataframe
      # Question: using full fit or not?
      fit_coef <- coef(fit)
      
      results$detections[i] <- list(names(fit_coef[fit_coef[,1] != 0, ])) # extract names nonzero features
      
      # repetition                                 
      results$i[i] <- i
      
      # for reference, the significant features
      results$sig_features[i] <- list(sapply(simdat$sig_features, function(x) paste0("V", x)))
      
      # for reference, number of features
      results$p[i] <- p
      
      # although "auc" is chosen as type.measure in cv.glmnet, this is not always possible
      # it may be good to keep track of this as well
      results$measure_cv[i] <- cvfit$name
      
      # print progress to the console
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }))
  results
}

