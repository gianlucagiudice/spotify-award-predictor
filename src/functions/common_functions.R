### ------------- Constants --------------
NUM_CORES <- detectCores()

train_target_model <- function(dataframe, method, tune_grid = NULL, tr_control = NULL,
                               seed, n_folds, train_model = TRUE,
                               dump_model = TRUE, num_cores = 1){
    
    ## Start cluster
    cl <- makePSOCKcluster(num_cores)
    registerDoParallel(cl)
    
    ## Train model
    filename = paste(method, "_report.RData", sep="")
    if (TRAIN_MODEL){
        training_report = cross_validation(dataframe = dataframe,
                                           method = method,
                                           seed = seed,
                                           tune_grid = tune_grid,
                                           tr_control = tr_control,
                                           n_folds = n_folds)
        if (DUMP_MODEL){
            ## Save report
            save(training_report, file=filename)
        }
    }else{
        load(filename)
    }
    
    ## Stop cluster
    stopCluster(cl)
    
    ## Performance
    performance.positive_folds = training_report[[1]]
    performance.negative_folds = training_report[[2]]
    ## Class performance
    performance.positive = combine_folds_performance(performance.positive_folds)
    performance.negative = combine_folds_performance(performance.negative_folds)
    plot_class_performance(performance.positive[5:7],
                           performance.negative[5:7],
                           method)
    ## Confusion matrix
    confusion_matrix = combine_folds_cm(performance.positive_folds)
    plot_performance(confusion_matrix,
                     performance.positive,
                     performance.negative,
                     method)
    plot_cm(confusion_matrix, method)
    
    # AUC
    #opt_cut = plot_auc(dataframe, 'radial')
    #print("Optimal cutoff:")
    #print(opt_cut)
}

cross_validation <- function(dataframe, method, tune_grid = NULL, tr_control = NULL, seed, n_folds){
    set.seed(seed)
    fold_indexes = createFolds(dataframe$award, k = n_folds)

    performance.positive = c()
    performance.negative = c()

    predictions = c()
    references = c()
    
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

        trained_model = train(award ~ .,
                              data = dataframe[train_idx,],
                              method = method,
                              tuneGrid = tune_grid,
                              trControl = tr_control)
        
        ## Save predictions for each fold
        pred = predict(trained_model, dataframe[test_idx, ], type = "prob")
        
        predictions <- c(predictions, list(pred))
        references <- c(references, dataframe[test_idx, "award"])
        
        ## Evaluate fold performance
        fold.positive_performance = list(evaluate_performance(trained_model,
                                                              dataframe[test_idx, ],
                                                              POSITIVE_CLASS_NAME))
        fold.negative_performance = list(evaluate_performance(trained_model,
                                                              dataframe[test_idx, ],
                                                              NEGATIVE_CLASS_NAME))

        ## Append new performance
        performance.positive = c(performance.positive, fold.positive_performance)
        performance.negative = c(performance.negative, fold.negative_performance)
    }

    return(list(performance.positive, performance.negative, predictions, references))
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
plot_class_performance <- function(positive, negative, method){
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
    
    filename = paste(method, "_class_performance.png", sep="")
    ggsave(filename, plot = last_plot(), path = "images",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

## Plot overall performance
plot_performance <- function(cm, positive, negative, method){
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
    
    filename = paste(method, "_performance.png", sep="")
    ggsave(filename, plot = last_plot(), path = "images",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

## Plot confusion matrix
plot_cm <- function(cm, method){
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
    
    filename = paste(method, "_cm.png", sep="")
    ggsave(filename, plot = last_plot(), path = "images",
           scale = 0.5, dpi = floor(DPI), limitsize = TRUE)
}

funzione_roc <- function (dataframe, method, class, n_folds, repeats, line_color) {
    ## Split train test set
    set.seed(SEED)
    ind = sample(2, nrow(dataframe), replace = TRUE, prob=c(0.7, 0.3))
    trainset <- dataframe[ind == 1,]
    testset <- dataframe[ind == 2,]

    control <- trainControl(method = "repeatedcv", number = n_folds,repeats = repeats,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
    
    trained_model <- train(award ~ .,
                           data = trainset,
                           method = method,
                           metric = "ROC",
                           trControl = control)

    trained_model.probs <- predict(trained_model, testset, type = "prob")

    trained_model.ROC <- roc(response = testset [,c("award")],
                             predictor = trained_model.probs[, class],
                             levels = levels(testset[,c("award")]))

    plot(trained_model.ROC, type = "S", col= line_color)

    return(trained_model)

}
