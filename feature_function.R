library(randomForest)
library(caret)
library(pbapply)
library(parallel)

rfeat_function = function(feature_data,
                          target_data,
                          feature_idx,
                          train_size_perc = 80) {


  coin_number = dim(feature_data)[1]
  mse_list    = numeric(coin_number)

  train_size = floor(dim(target_data)[2] * (train_size_perc/100))
  train_idx  = 1:train_size

  feature_names = names(feature_data[1,1,feature_idx])

  importance_matrix = matrix(0, nrow = dim(feature_data)[1], ncol = dim(feature_data)[3])

  pb = txtProgressBar(min = 0, max = coin_number, style = 3)

  for (i in 1:coin_number) {
    features_i = feature_data[i,,feature_idx]

    target_data_i = exp(target_data[i,]) - 1

    train_x  = features_i[train_idx, ]
    train_y  = target_data_i[train_idx]

    test_x   = features_i[-train_idx, ]
    test_y   = target_data_i[-train_idx]

    rf_model    = randomForest(x = train_x, y = train_y, ntree = 100)
    predictions = predict(rf_model, test_x)

    importance_matrix[i, ] = importance(rf_model)[,1]

    mse_list[i] = mean((test_y - predictions)^2)

    setTxtProgressBar(pb, i)
  }
  close(pb)

  return(list(MSE=mse_list,Importance=importance_matrix))
}


rfeat_function_parallel = function(feature_data,
                                   target_data,
                                   feature_idx,
                                   train_size_perc = 80,
                                   n_cores = detectCores() - 1) {

  coin_number = dim(feature_data)[1]
  train_size  = floor(dim(target_data)[2] * (train_size_perc/100))
  train_idx   = 1:train_size

  cl = makeCluster(n_cores)

  clusterExport(cl, varlist = c("feature_data", "target_data", "feature_idx", "train_idx"),
                envir = environment())
  clusterEvalQ(cl, library(randomForest))

  worker_func = function(i) {
    features_i    = feature_data[i, , feature_idx]
    target_data_i = exp(target_data[i, ]) - 1

    train_x = features_i[train_idx, ]
    train_y = target_data_i[train_idx]

    test_x  = features_i[-train_idx, ]
    test_y  = target_data_i[-train_idx]

    rf_model    = randomForest(x = train_x, y = train_y, ntree = 100)
    predictions = predict(rf_model, test_x)

    return(list(
      mse = mean((test_y - predictions)^2),
      imp = importance(rf_model)[, 1]
    ))
  }

  results = pblapply(1:coin_number, worker_func, cl = cl)

  stopCluster(cl)

  mse_list = sapply(results, function(x) x$mse)
  importance_matrix = do.call(rbind, lapply(results, function(x) x$imp))

  return(list(MSE = mse_list, Importance = importance_matrix))
}


##########################################################
################# Discrete random forest #################
##########################################################

rfeat_function_discrete = function(feature_data,
                                   target_data,
                                   feature_idx,
                                   train_size_perc = 80) {


  coin_number = dim(feature_data)[1]

  train_size = floor(dim(target_data)[2] * (train_size_perc/100))
  train_idx  = 1:train_size

  feature_names = names(feature_data[1,1,feature_idx])

  importance_matrix = matrix(0,
                             nrow = dim(feature_data)[1],
                             ncol = length(feature_idx))

  stat_matrix = matrix(0, nrow = dim(feature_data)[1], ncol = 4)

  pb = txtProgressBar(min = 0, max = coin_number, style = 3)

  for (i in 1:coin_number) {
    features_i = feature_data[i,,feature_idx]

    target_return_i = exp(target_data[i,]) - 1
    target_signal_i = ifelse(target_return_i > 0.002, 1, 0)

    if (sum(target_signal_i[-train_idx]) > 10) {
      target_data_i = as.factor(target_signal_i)

      train_x  = features_i[train_idx, ]
      train_y  = target_data_i[train_idx]

      test_x   = features_i[-train_idx, ]
      test_y   = target_data_i[-train_idx]

      n_pos = sum(train_y == "1")
      n_neg = sum(train_y == "0")

      rf_model = randomForest(x = train_x, y = train_y,
                              ntree = 200,
                              sampsize = c("0" = n_pos, "1" = n_pos))
      predictions = predict(rf_model, test_x)

      predictions = factor(predictions, levels = c("0", "1"))
      test_y = factor(test_y, levels = c("0", "1"))

      importance_matrix[i, ] = importance(rf_model)[,1]

      stats = confusionMatrix(predictions, test_y, positive = "1")

      prec   = stats$byClass["Precision"]
      rec    = stats$byClass["Recall"]
      f1     = stats$byClass["F1"]
      actual_pos = sum(test_y == "1")

      if (is.nan(f1)) {
        stat_matrix[i,] = c(0,0,0,actual_pos)
      } else {
        stat_matrix[i, ] = c(as.numeric(prec), as.numeric(rec), as.numeric(f1), actual_pos)
      }

    } else {
      stat_matrix[i,] = c(0,0,0,0)
      importance_matrix[i, ] = rep(0,length(feature_idx))
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  return(list(stats=stat_matrix,Importance=importance_matrix))
}
