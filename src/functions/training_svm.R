# --------- Functions ---------
# Find best cost parameter using 10-folds cross validation
find_best_parameters <- function(df, kernel) {
  tuned = tune.svm(award~., data=df.out,
                   kernel=kernel,
                   tune.control(cross = 5),
                   cost=COST_LIST, gamma = GAMMA_LIST)
  return(tuned)
}

# Train support vector machine classifier using cross validation
train_svm_cv <- function(df, folds){
  performance.positive = c()
  performance.negative = c()
  
  for (i in 1:length(folds)){
    # Build training and test sets
    test_idx = folds[[i]]
    train_idx = c()
    for (j in 1:length(folds)){
      if (j != i){
        train_idx = c(train_idx, folds[[j]])
      }
    }
    
    print(paste("Training SVM - fold ", i, "/", length(folds)))
    svm.trained_cv = train(award ~ ., data = df[train_idx,],
                           method = "svmRadial",
                           tuneGrid=expand.grid(
                             C=COST_LIST,
                             sigma=GAMMA_LIST
                           ),
                           num.threads = NUM_CORES)
    # Evaluate fold performance
    fold.positive_performance = list(evaluate_performance(
      svm.trained_cv, df[test_idx, ], "TRUE"))
    fold.negative_performance = list(evaluate_performance(
      svm.trained_cv, df[test_idx, ], "FALSE"))
    # Append new performance
    performance.positive = c(performance.positive, fold.positive_performance)
    performance.negative = c(performance.negative, fold.negative_performance)
  }
  
  return(list(performance.positive, performance.negative))
}

