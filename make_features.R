library(abind)
library(here)

source(here('functions.R'))

data = readRDS(here('data.rds'))

features_list <- list(
  sma_close_dist   = sma_close_dist(data, n = 10),
  ema_close_dist   = ema_close_dist(data, n = 20),
  acceleration     = acceleration(data, short = 5, long = 10),
  range_ratio      = range_ratio(data, window = 20),
  momentum_decay   = momentum_decay(data, window = 5, decay_rate = 0.5),
  sharpe_ratio     = sharpe_ratio(data, window = 20),
  cross_zscore     = cross_zscore(data),
  skewness_20      = skewness_20(data, window = 20),
  kurtosis_20      = kurtosis_20(data, window = 20),
  sortino_ratio    = sortino_ratio(data, window = 20),
  inside_bar       = inside_bar(data),
  outside_bar      = outside_bar(data),
  gap_filled       = gap_filled(data),
  vol_autocorr     = vol_autocorr(data, window = 20),
  volatility_ratio = volatility_ratio(data, short = 5, long = 20),
  price_position   = price_position(data, window = 20),
  var_95           = var_95(data, window = 20)
)

n_coins <- dim(data)[1]
n_time  <- dim(data)[2]
n_feats <- length(features_list)

features_array <- array(NA,
                        dim = c(n_coins, n_time, n_feats),
                        dimnames = list(
                          coins = dimnames(data)[[1]],
                          time  = dimnames(data)[[2]],
                          features = names(features_list)
                        ))

for (feat_name in names(features_list)) {
  features_array[,,feat_name] <- features_list[[feat_name]]
}

saveRDS(features_array, here('features.rds'))

