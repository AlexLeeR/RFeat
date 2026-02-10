# workable features

library(future.apply)
library(TTR)
library(e1071)
library(runner)
library(randomForest)
library(caret)

kline_data = readRDS(file = 'data.rds')

rsi_scalar = function(series) {
  delta = diff(series)

  gain = ifelse(delta > 0, delta, 0)
  loss = ifelse(delta < 0, -delta, 0)

  avg_gain = mean(gain)
  avg_loss = mean(loss)

  if (avg_loss == 0) {
    return(100)
  }

  rs <- avg_gain / avg_loss
  rsi_value = 100 - (100 / (1 + rs))

  return(rsi_value)
}

pb = txtProgressBar(min = 0, max = dim(kline_data)[1], style = 3)
feature_list = list()

for (c in 1:dim(kline_data)[1]) {
  lr_data  = kline_data[c,,5]
  mw_data  = runner(lr_data, k = 10)[10:length(lr_data)]

  features <- future_lapply(1:length(mw_data), function(idx) {
    x_val <- mw_data[[idx]]

    has_variance <- length(unique(x_val)) > 1

    m  = mean(x_val)
    mn = min(x_val)
    mx = max(x_val)

    if (has_variance) {
      s      = sd(x_val)
      sharpe = m / s
      kurt   = e1071::kurtosis(x_val)
      skew   = e1071::skewness(x_val)

      fit     <- lm(x_val ~ seq_along(x_val))
      s_fit   <- summary(fit)
      slope   <- coef(fit)[2]
      r_val   <- sqrt(s_fit$r.squared) * sign(slope)
      p_val   <- s_fit$coefficients[2, 4]
      std_err <- s_fit$coefficients[2, 2]
    } else {
      s = 0; sharpe = 0; kurt = 0; skew = 0
      slope = 0; r_val = 0; p_val = 1; std_err = 0
    }


    maad = mean(abs(x_val - m))
    mad  = median(abs(x_val - m))
    rvol = sqrt(sum(x_val^2))
    rsi  = rsi_scalar(x_val)
    ema  = tail(TTR::EMA(x_val, n = 3), 1)

    if(is.na(ema)) ema <- m

    return(c(m, s, sharpe, mn, mx, kurt, skew, maad, mad, rvol, rsi, ema,
             slope, 0, r_val, p_val, std_err))
  })

  feature_list[[c]] = features

  setTxtProgressBar(pb, c)
}
close(pb)

feature_array0 <- simplify2array(lapply(feature_list, simplify2array))
feature_array  <- aperm(feature_array0,c(3,2,1))

target_data = kline_data[,10:dim(kline_data)[2],8]

pb = txtProgressBar(min = 0, max = dim(kline_data)[1], style = 3)
k  = 0

train_size_perc = 80
train_size = floor(dim(target_data)[2] * (train_size_perc/100))
train_idx  = 1:train_size

importance_matrix = matrix(0,nrow=dim(feature_array)[1],ncol=dim(feature_array)[3])

coin_number = dim(feature_array)[1]
mse_list    = numeric(coin_number)

feature_list = list()

dimnames(feature_array)[[3]] <- c("mean", "sd", "sharpe", "min", "max", "kurt", "skew",
                                  "maad", "mad", "rvol", "rsi", "ema",
                                  "slope", "intercept", "r_val", "p_val", "std_err")

for (c in 1:dim(kline_data)[1]) {

  features_c = feature_array[c, , , drop = FALSE]
  features_c = matrix(features_c, nrow = dim(features_c)[2], ncol = dim(features_c)[3])
  colnames(features_c) <- dimnames(feature_array)[[3]]

  features_c[!is.finite(features_c)] <- 0

  target_data_c = exp(target_data[c,]) - 1

  target_data_c[!is.finite(target_data_c)] <- 0

  train_x  = features_c[train_idx, , drop = FALSE]
  train_y  = target_data_c[train_idx]

  test_x   = features_c[-train_idx, , drop = FALSE]
  test_y   = target_data_c[-train_idx]

  rf_model    = randomForest(x = train_x, y = train_y, ntree = 100, importance = TRUE)

  predictions = predict(rf_model, test_x)

  imp_val = importance(rf_model)
  importance_matrix[c, ] = imp_val[, 1]

  mse_list[c] = mean((test_y - predictions)^2, na.rm = TRUE)

  setTxtProgressBar(pb, c)
}

imp7 = which(rank(apply(importance_matrix,2,mean))>9)
mse_list2    = numeric(coin_number)

all_feature_names <- dimnames(feature_array)[[3]]
top_feature_names <- all_feature_names[imp7]

for (c in 1:dim(kline_data)[1]) {

  features_c_raw = feature_array[c, , imp7, drop = FALSE]

  features_c = matrix(features_c_raw,
                      nrow = dim(features_c_raw)[2],
                      ncol = dim(features_c_raw)[3])

  colnames(features_c) <- top_feature_names

  features_c[!is.finite(features_c)] <- 0

  target_data_c = exp(target_data[c,]) - 1
  target_data_c[!is.finite(target_data_c)] <- 0

  train_x  = features_c[train_idx, , drop = FALSE]
  train_y  = target_data_c[train_idx]

  test_x   = features_c[-train_idx, , drop = FALSE]
  test_y   = target_data_c[-train_idx]

  rf_model    = randomForest(x = train_x, y = train_y, ntree = 100, importance = TRUE)

  predictions = predict(rf_model, test_x)

  mse_list2[c] = mean((test_y - predictions)^2, na.rm = TRUE)

  setTxtProgressBar(pb, c)
}



################################
#### Discrete random forest ####
################################


target_signal_all = kline_data[, 10:dim(kline_data)[2], 7]
coin_number = dim(feature_array)[1]

importance_matrix_clf = matrix(0, nrow = coin_number, ncol = 17)

for (c in 1:coin_number) {
  features_c = feature_array[c, , , drop = FALSE]
  features_c = matrix(features_c, nrow = dim(features_c)[2], ncol = dim(features_c)[3])
  colnames(features_c) <- dimnames(feature_array)[[3]]
  features_c[!is.finite(features_c)] <- 0

  train_y_clf = as.factor(target_signal_all[c, train_idx])
  train_x_clf = features_c[train_idx, ]

  rf_imp = randomForest(x = train_x_clf, y = train_y_clf, ntree = 50, importance = TRUE)
  importance_matrix_clf[c, ] = importance(rf_imp)[, 1]
}

imp7_new = which(rank(colMeans(importance_matrix_clf)) > 9)
top_feature_names_clf = dimnames(feature_array)[[3]][imp7_new]

metrics_matrix = matrix(0, nrow = coin_number, ncol = 4)
colnames(metrics_matrix) <- c("Precision", "Recall", "F1", "Support")

pb = txtProgressBar(min = 0, max = coin_number, style = 3)

for (c in 1:coin_number) {

  features_c = feature_array[c, , imp7_new, drop = FALSE]
  features_c = matrix(features_c, nrow = dim(features_c)[2], ncol = dim(features_c)[3])
  colnames(features_c) <- top_feature_names_clf
  features_c[!is.finite(features_c)] <- 0

  target_c = as.factor(target_signal_all[c, ])

  train_x = features_c[train_idx, , drop = FALSE]
  train_y = target_c[train_idx]
  test_x  = features_c[-train_idx, , drop = FALSE]
  test_y  = target_c[-train_idx]

  rf_model = randomForest(x = train_x, y = train_y, ntree = 100)

  preds = predict(rf_model, test_x)

  tp = sum(preds == "1" & test_y == "1")
  fp = sum(preds == "1" & test_y == "0")
  fn = sum(preds == "0" & test_y == "1")

  precision = if ((tp + fp) > 0) tp / (tp + fp) else 0

  recall = if ((tp + fn) > 0) tp / (tp + fn) else 0

  f1 = if ((precision + recall) > 0) (2 * precision * recall) / (precision + recall) else 0

  support = sum(test_y == "1")

  metrics_matrix[c, ] = c(precision, recall, f1, support)

  setTxtProgressBar(pb, c)
}
close(pb)
