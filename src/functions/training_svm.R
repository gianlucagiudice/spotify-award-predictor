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


# Evaluate model performance
evaluate_performance <- function(model, testset, class) {
  pred = predict(model, testset)
  table(pred, testset$award) 
  result = confusionMatrix(pred, testset$award,
                           mode = "everything", positive=class)
  return(result)
}

                                        # Plot model performance
plot_class_performance <- function(positive, negative){
    precision <- c(positive[1], negative[1])
    recall <- c(positive[2], negative[2])
    f1 <- c(positive[3], negative[3])
    metric <- c("precision", "precision", "recall", "recall", "f1", "f1")
    score <-c(positive[1], negative[1],
              positive[2], negative[2],
              positive[3], negative[3])
    award <- c("TRUE", "FALSE","TRUE", "FALSE","TRUE", "FALSE")
    data.plot = data.frame(metric, score, award)
    
    ggplot(data=data.plot,
           aes(x = factor(metric, level = c('precision', 'recall', 'f1')),
               y = score, fill=factor(award))) +
        ggtitle("Perfomance - award vs not award") +
        labs(fill="Award") +
        xlab("Metric") + ylab("Score") + 
        geom_col(colour="black", position="dodge") +
        scale_y_continuous(breaks=seq(0, 1, 0.025)) +
        theme(plot.title = element_text(hjust = 0.5))
    
    ggsave("svm_class_performance.png", plot = last_plot(), path = "images",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}


                                        # Combine folds to extract by-class performance
combine_folds_performance <- function(performance_folds){
  folds = c()
  for (i in 1:length(performance_folds)){
    folds = c(folds, list(performance_folds[[i]]$byClass))
  }
  values = matrix(unlist(folds), nrow=11)
  values = rowSums(values) / length(performance_folds)
  return(values)
}


# Combine folds to extract the confusion matrix
combine_folds_cm <- function(performance_folds){
  cm_list = c()
  for (i in 1:length(performance_folds)){
    cm_list = c(cm_list, list(performance_folds[[i]][[2]]))
  }
  acc = cm_list[[1]]
  for (i in 2:length(cm_list)){
    acc = acc + cm_list[[i]]
  }
  return(acc)
}


# Plot overall performance
plot_performance <- function(cm, positive, negative){
  # Accuracy
  acc = (cm[1] + cm[4]) / (cm[1] + cm[2] + cm[3] + cm[4])
  # Macro score
  prec = (positive[5] + negative[5]) / 2
  recall = (positive[6] + negative[6]) / 2
  f1 = 2 * (prec * recall) / (prec + recall)
  # Dataframe for plot
  metric <- c("Accuracy", "Precision", "Recall", "F1")
  score <- c(acc, prec, recall, f1)
  data.plot = data.frame(metric, score)
  
  ggplot(data=data.plot,
         aes(x = factor(metric,
                        level = c('Accuracy', 'Precision', 'Recall', 'F1')),
             y = score)) +
    ggtitle("Perfomance - Overall (macro average)") +
    labs(fill="Award") +
    xlab("Metric") + ylab("Score") + 
    geom_col(colour="black", fill="blue", position="dodge") +
    scale_y_continuous(breaks=seq(0, 1, 0.025)) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave("svm_performance.png", plot = last_plot(), path = "images",
         scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}


# Plot confusion matrix
plot_cm <- function(cm){
  cm = as.data.frame(confusion_matrix)
  ggplot(data = cm,
         aes(x = factor(Reference, level = c('TRUE', 'FALSE')),
             y = factor(Prediction, level = c('FALSE', 'TRUE')))) +
    geom_tile(aes(fill = Freq), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue",
                        limits=c(0, max(cm$Freq))) +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    ggtitle("Confusion matrix") +
    xlab("Reference") + ylab("Prediction") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("svm_cm.png", plot = last_plot(), path = "images",
         scale = 0.5, dpi = floor(DPI), limitsize = TRUE)
}


# AUC
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


# Optimal cutoff
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
