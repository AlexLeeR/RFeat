source('R/feature_function.R')

data = readRDS('data/data.rds')
features = readRDS('data/features.rds')
full_idx = which(!is.na(apply(features,2,sum)))
full_features = 1:dim(features)[3]

features_run = rfeat_function(feature_data = features[,full_idx,],
                              target_data  = data[,full_idx,8],
                              feature_idx  = full_features)

ranks = rank(apply(features_run$Importance,2,mean))

a = 0

mean_MSEs = c()

while (a < 16) {
  feature_ranks = which(rank(apply(features_run$Importance,2,mean)) > a)
  print(length(feature_ranks))
  features_run_a = rfeat_function_parallel(feature_data = features[,full_idx,],
                                           target_data  = data[,full_idx,8],
                                           feature_idx  = feature_ranks)

  mean_MSEs = c(mean_MSEs,mean(features_run_a$MSE))
  a=a+1
  cat("\n",a,"iteration complete!","\n")
}


##########################################################
################# Discrete random forest #################
##########################################################

features_run = rfeat_function_discrete(features[,full_idx,],data[,full_idx,8],
                                       feature_idx  = 1:dim(features)[3])

a = 0

f1_scores = c()
f1_0s     = c()

while (a < 17) {
  feature_ranks = which(rank(apply(features_run$Importance, 2, mean)) > a)

  features_run_a = rfeat_function_discrete(feature_data = features[,full_idx,],
                                           target_data  = data[,full_idx,8],
                                           feature_idx  = feature_ranks)

  f1_scores = c(f1_scores, mean(features_run_a$stats[,3]))
  f1_0s     = c(f1_0s, length(which(features_run_a$stats[,3] == 0)))

  a = a + 1
}

