### TODO forse la creazione dei fold puó essere spostata dal main a questa funzione, i fold singolarmente non vengono mai utilizzati tranne che in questa funzione.

### Performs a k-cross validation (where k is the number of fold_indexes)
### The training_function first argument must be the training set, the rest of the arguments must be supplied in the training_function_args list


# ------------- Constants --------------
NUM_CORES <- detectCores()


cross_validation <- function(dataframe, method, tune_grid, seed, n_folds, num_threads){
    set.seed(seed)
    fold_indexes = createFolds(df.out$award, k = N_FOLDS)

    performance.positive = c()
    performance.negative = c()

    for (i in 1:length(fold_indexes)){
        ## Build training and test sets
        test_idx = fold_indexes[[i]]
        train_idx = c()
        for (j in 1:length(fold_indexes)){
            if (j != i){
                train_idx = c(train_idx, fold_indexes[[j]])
            }
        }

        print(paste("Training ", method, " - fold ", i, "/", length(fold_indexes)))
        ## trained_model = training_function(dataframe[train_idx,])
        
        #trained_model = do.call(training_function, append(list(dataframe[train_idx,]),
        #                                                  training_function_args))

        trained_model = train(award ~ ., data = df[train_idx,],
                           method = method,
                           tuneGrid = tune_grid,
                           num.threads = num_threads)                                           
        ## Evaluate fold performance
        fold.positive_performance = list(evaluate_performance(
            trained_model, dataframe[test_idx, ], "TRUE"))
        fold.negative_performance = list(evaluate_performance(
            trained_model, dataframe[test_idx, ], "FALSE"))
        ## Append new performance
        performance.positive = c(performance.positive, fold.positive_performance)
        performance.negative = c(performance.negative, fold.negative_performance)
    }

    return(list(performance.positive, performance.negative))
}

### Evaluate model performance
evaluate_performance <- function(model, testset, class) {
    pred = predict(model, testset)
    table(pred, testset$award) 
    result = confusionMatrix(pred, testset$award,
                             mode = "everything", positive=class)
    return(result)
}

### Combine folds to extract by-class performance
combine_folds_performance <- function(performance_folds){
    folds = c()
    for (i in 1:length(performance_folds)){
        folds = c(folds, list(performance_folds[[i]]$byClass))
    }
    values = matrix(unlist(folds), nrow=11)
    values = rowSums(values) / length(performance_folds)
    return(values)
}

### Combine folds to extract the confusion matrix
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

## Plot model performance
plot_class_performance_generic <- function(positive, negative, filename){
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
    
    ggsave(filename, plot = last_plot(), path = "images",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

## Plot overall performance
plot_performance_generic <- function(cm, positive, negative, filename){
    ## Accuracy
    acc = (cm[1] + cm[4]) / (cm[1] + cm[2] + cm[3] + cm[4])
    ## Macro score
    prec = (positive[5] + negative[5]) / 2
    recall = (positive[6] + negative[6]) / 2
    f1 = 2 * (prec * recall) / (prec + recall)
    ## Dataframe for plot
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
    ggsave(filename, plot = last_plot(), path = "images",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

## Plot confusion matrix
plot_cm_generic <- function(cm, filename){
    cm = as.data.frame(cm)
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
    
    ggsave(filename, plot = last_plot(), path = "images",
           scale = 0.5, dpi = floor(DPI), limitsize = TRUE)
}
