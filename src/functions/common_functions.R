### TODO forse la creazione dei fold puó essere spostata dal main a questa funzione, i fold singolarmente non vengono mai utilizzati tranne che in questa funzione.

### Performs a k-cross validation (where k is the number of fold_indexes)
### 
cross_validation_generic <- function(dataframe, training_function, fold_indexes, technique_name_string){
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

        print(paste("Training ", technique_name_string, " - fold ", i, "/", length(fold_indexes)))
        trained_model = training_function(dataframe[train_idx,])

        ## Evaluate fold performance
        fold.positive_performance = list(evaluate_performance(
            trained_model, dataframe[test_idx, ], "TRUE"))
        fold.negative_performance = list(evaluate_performance(
            trained_model, dataframe[test_idx, ], "FALSE"))
                                        # Append new performance
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
