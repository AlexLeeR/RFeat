library(RcppRoll)
library(moments)
library(zoo)
library(TTR)

fast_lag = function(mat, n) {
  if (n == 0) return(mat)
  res = matrix(NA, nrow(mat), ncol(mat))
  if (ncol(mat) > n) res[, (n + 1):ncol(mat)] = mat[, 1:(ncol(mat) - n)]
  return(res)
}

fast_zscore = function(mat) {
  means = colMeans(mat, na.rm = TRUE)
  sds = apply(mat, 2, sd, na.rm = TRUE)
  sds[is.na(sds) | sds == 0] = 1
  z = t((t(mat) - means) / sds)
  return(fast_lag(z, 1))
}


sma_close_dist = function(arr, n = 10) {
  cp = arr[, , "close"]
  sma = t(apply(cp, 1, function(x) roll_meanr(x, n, fill = NA)))
  return((fast_lag(cp, 1) / fast_lag(sma, 2)) - 1)
}

ema_close_dist = function(arr, n = 20) {
  cp = arr[, , "close"]
  ema = t(apply(cp, 1, function(x) TTR::EMA(x, n = n)))
  return((fast_lag(cp, 1) / fast_lag(ema, 2)) - 1)
}

acceleration = function(arr, short = 5, long = 10) {
  ret = arr[, , "log_ret"]
  mom = t(apply(ret, 1, function(x) roll_sumr(x, n = short, fill = NA)))
  return(fast_lag(mom, 1) - fast_lag(mom, short + 1))
}

range_ratio = function(arr, window = 20) {
  rng = arr[, , "high"] - arr[, , "low"]
  avg = t(apply(rng, 1, function(x) roll_meanr(x, window, fill = NA)))
  return(fast_lag(rng, 1) / (fast_lag(avg, 1) + 1e-10))
}

momentum_decay = function(arr, window = 5, decay_rate = 0.5) {
  ret = arr[, , "log_ret"]
  w   = exp(-seq(0, window - 1) * decay_rate)
  mom = t(apply(ret, 1, function(x) roll_sumr(x, window, weights = rev(w), fill = NA)))
  return(fast_lag(mom, 1))
}

sharpe_ratio = function(arr, window = 20, rf = 0) {
  ret = arr[, , "log_ret"]
  mu  = t(apply(ret, 1, function(x) roll_meanr(x, window, fill = NA)))
  sig = t(apply(ret, 1, function(x) roll_sdr(x, window, fill = NA)))
  return((fast_lag(mu, 2) - rf) / (fast_lag(sig, 2) + 1e-10))
}

cross_zscore = function(arr) fast_zscore(arr[, , "log_ret"])

safe_skewness = function(x) {
  x_clean = x[!is.na(x)]
  if(length(unique(x_clean)) <= 1 || length(x_clean) < 3) return(0)
  return(moments::skewness(x_clean))
}

safe_kurtosis = function(x) {
  x_clean = x[!is.na(x)]
  if(length(unique(x_clean)) <= 1 || length(x_clean) < 4) return(0)
  return(moments::kurtosis(x_clean))
}

skewness_20 = function(arr, window = 20) {
  skew = t(apply(arr[, , "log_ret"], 1, function(x) {
    rollapplyr(x, window, safe_skewness, fill = NA)
  }))
  return(fast_lag(skew, 2))
}

kurtosis_20 = function(arr, window = 20) {
  kurt = t(apply(arr[, , "log_ret"], 1, function(x) {
    rollapplyr(x, window, safe_kurtosis, fill = NA)
  }))
  return(fast_lag(kurt, 2))
}

sortino_ratio = function(arr, window = 20) {
  ret = arr[, , "log_ret"]
  mu  = t(apply(ret, 1, function(x) roll_meanr(x, window, fill = NA)))
  dsig = t(apply(ret, 1, function(x) {
    dside = x; dside[dside > 0] = 0
    roll_sdr(dside, window, fill = NA)
  }))
  return(fast_lag(mu, 2) / (fast_lag(dsig, 2) + 1e-10))
}

inside_bar = function(arr) {
  hi = arr[, , "high"]; lo = arr[, , "low"]
  ib = (hi <= fast_lag(hi, 1) & lo >= fast_lag(lo, 1)) * 1
  return(fast_lag(ib, 1))
}

outside_bar = function(arr) {
  hi = arr[, , "high"]; lo = arr[, , "low"]
  ob = (hi >= fast_lag(hi, 1) & lo <= fast_lag(lo, 1)) * 1
  return(fast_lag(ob, 1))
}

gap_filled = function(arr) {
  op = arr[, , "open"]; cl = arr[, , "close"]
  hi = arr[, , "high"]; lo = arr[, , "low"]
  gap = log(fast_lag(op, 1) / (fast_lag(cl, 2) + 1e-10))
  filled = matrix(0, nrow(op), ncol(op))
  filled[gap > 0  & lo <= fast_lag(cl, 2)] = 1
  filled[gap < 0  & hi >= fast_lag(cl, 2)] = 1
  return(filled)
}

vol_autocorr = function(arr, window = 20) {
  abs_ret = abs(arr[, , "log_ret"])
  res = t(apply(abs_ret, 1, function(x) {
    rollapplyr(x, window, function(z) {

      z_clean = z[!is.na(z)]

      if(length(z_clean) < 2) return(NA)

      if(length(unique(z_clean)) == 1) return(0)

      tryCatch({
        cor(z_clean[-1], z_clean[-length(z_clean)], use = "complete.obs")
      }, error = function(e) {
        return(0)
      }, warning = function(w) {
        return(0)
      })
    }, fill = NA)
  }))
  return(fast_lag(res, 1))
}

volatility_ratio = function(arr, short = 5, long = 20) {
  ret = arr[, , "log_ret"]
  v_s = t(apply(ret, 1, function(x) roll_sdr(x, short, fill = NA)))
  v_l = t(apply(ret, 1, function(x) roll_sdr(x, long, fill = NA)))
  return(fast_lag(v_s, 1) / (fast_lag(v_l, 1) + 1e-10))
}

days_since_low = function(arr, lookback = 50) {
  cp = arr[, , "close"]
  res = t(apply(cp, 1, function(x) {
    rollapplyr(x, lookback, function(z) {
      if(all(is.na(z))) return(NA)
      lookback - max(which(z == min(z, na.rm=T)))
    }, fill = NA)
  }))
  return(fast_lag(res, 1))
}

price_position = function(arr, window = 20) {
  cp = arr[, , "close"]
  hi = t(apply(cp, 1, function(x) roll_maxr(x, window, fill = NA)))
  lo = t(apply(cp, 1, function(x) roll_minr(x, window, fill = NA)))
  return((fast_lag(cp, 1) - fast_lag(lo, 1)) / (fast_lag(hi, 1) - fast_lag(lo, 1) + 1e-10))
}

var_95 = function(arr, window = 20) {
  v95 = t(apply(arr[, , "log_ret"], 1, function(x) rollapplyr(x, window, quantile, probs = 0.05, na.rm=T, fill = NA)))
  return(fast_lag(v95, 2))
}
