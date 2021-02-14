# --------- Libraries ---------
library(parallel)
library(caret)

libraries = c("e1071", "caret", "ROCR", "C50", "pROC", "parallel",
              "libcoin", "kernlab")

if (INSTALL_LIBRARIES){
  install.packages(libraries, dependencies = TRUE, character.only = TRUE)
}
for (library in libraries){
  library(library, character.only = TRUE)
}


# ------------- Constants --------------
NUM_CORES <- detectCores()
COST_LIST = 10^(-3:1)
GAMMA_LIST = 10^(-5:-1)


DPI <- 300
SCALE = 0.75


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

## AUC
plot_auc <- function(df.out, kernel){
  # ========== MOLTO IMPORTANTE !!!! ==========
  # TODO: Classe positiva e negativa
  # ========== MOLTO IMPORTANTE !!!! ==========
  set.seed(SEED)
  
  #ind_sub = sample(2, nrow(df.out), replace = TRUE, prob=c(0.1, 0.9))
  #df.out = df.out[ind_sub == 1]
  
  ind = sample(2, nrow(df.out), replace = TRUE, prob=c(0.7, 0.3))
  trainset = df.out[ind == 1,]
  testset = df.out[ind == 2,] 
  # Performance
  hyperparameter = find_best_parameters(testset, kernel)
  svmfit=svm(award~ ., data=trainset, prob=TRUE, kernel = kernel,
             gamma=hyperparameter$best.parameters$gamma,
             cost = hyperparameter$best.parameters$c)
  # Probabilmente bisogna cambiare parametro probability = TRUE
  pred=predict(svmfit,
               testset[, !names(testset) %in% c("award")], probability=TRUE) 
  pred.prob = attr(pred, "probabilities")
  pred.to.roc = pred.prob[, 1]
  #Plot
  pred.rocr = prediction(pred.to.roc, testset$award)
  perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff") 
  perf.tpr.rocr = performance(pred.rocr, "tpr","fpr") 
  opt_cut = opt.cut(perf.tpr.rocr, pred.rocr)
  # ROC curve
  png('images/svm_auc.png')
  plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
  abline(a=0, b=1)
  dev.off()
  # Best cutoff
  acc.perf = performance(pred.rocr, measure = "acc")
  png('images/svm_cutoff_acc.png')
  plot(acc.perf, main = "Accurancy to changes in cutoff")
  dev.off()
  
  return(opt_cut)
}

## Optimal cutoff
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}
