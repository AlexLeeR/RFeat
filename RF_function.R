num_iterations = dim(target_signal)[1]
mse_list = numeric(num_iterations)

feature_names = names(features[1,1,])

importance_matrix = matrix(0, nrow = dim(features)[2], ncol = dim(features)[3])

pb = txtProgressBar(min = 0, max = num_iterations, style = 3)

for (i in 1:num_iterations) {
  features_i = features[i,,]

  target_signal_i = exp(target_signal[i,]) - 1

  train_x  = features_i[train_idx, ]
  train_y  = target_signal_i[train_idx]
  test_x   = features_i[-train_idx, ]
  test_y   = target_signal_i[-train_idx]

  rf_model    = randomForest(x = train_x, y = train_y, ntree = 100)
  predictions = predict(rf_model, test_x)

  importance_matrix[i, ] = importance(rf_model)[,1]

  mse_list[i] = mean((test_y - predictions)^2)

  setTxtProgressBar(pb, i)
}
close(pb)
