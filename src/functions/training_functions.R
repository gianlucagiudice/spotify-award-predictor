### --------- Libraries ---------
libraries = c("e1071", "caret", "ROCR", "C50", "pROC", "parallel",
              "libcoin", "kernlab", "doParallel")

if (INSTALL_LIBRARIES){
    install.packages(libraries, dependencies = TRUE, character.only = TRUE)
}
for (library in libraries){
    library(library, character.only = TRUE)
}


### --------- Functions ---------
train_target_model <- function(dataframe, method, tune_grid = NULL,
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
    
    ## AUC
    probs = Reduce(union_all, training_report[[3]])
    references = training_report[[4]]
    references = resolve_label(dataframe, probs, references)
    ## Positive class
    opt_cut = plot_auc(probs, references, method, POSITIVE_CLASS_NAME)
    print("Optimal cutoff positive class:")
    print(opt_cut)
    ## Negative class
    opt_cut = plot_auc(probs, references, method, NEGATIVE_CLASS_NAME)
    print("Optimal cutoff negative class:")
    print(opt_cut)
}

cross_validation <- function(dataframe, method, tune_grid = NULL, seed, n_folds){
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
                              trControl = trainControl(classProbs = TRUE,
                                                       method = "cv",
                                                       number = 10))
        
        ## Save predictions for each fold
        pred = predict(trained_model, dataframe[test_idx, ], type = "prob")
        
        predictions <- c(predictions, list(pred))
        references <- c(references, dataframe[test_idx, "award"])
        
        ## Evaluate fold performance
        fold.positive_performance =
            list(evaluate_performance(trained_model,
                                      dataframe[test_idx, ],
                                      POSITIVE_CLASS_NAME))
        fold.negative_performance =
            list(evaluate_performance(trained_model,
                                      dataframe[test_idx, ],
                                      NEGATIVE_CLASS_NAME))

        ## Append new performance
        performance.positive = c(performance.positive, fold.positive_performance)
        performance.negative = c(performance.negative, fold.negative_performance)
    }

    return(list(performance.positive,
                performance.negative,
                predictions, references))
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
    award <- c(
        POSITIVE_CLASS_NAME, NEGATIVE_CLASS_NAME,
        POSITIVE_CLASS_NAME, NEGATIVE_CLASS_NAME,
        POSITIVE_CLASS_NAME, NEGATIVE_CLASS_NAME)
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

### Plot overall performance
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
        coord_flip() +
        xlab("Metric") + ylab("Score") + 
        geom_col(colour="black", fill="blue", position="dodge") +
        scale_y_continuous(breaks=seq(0, 1, 0.025)) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90))
    
    filename = paste(method, "_performance.png", sep="")
    ggsave(filename, plot = last_plot(), path = "images",
           height=15, width=25, units="cm",
           scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

### Plot confusion matrix
plot_cm <- function(cm, method){
    cm = as.data.frame(cm)
    
    ggplot(data = cm,
           aes(x = factor(Reference, level = c(POSITIVE_CLASS_NAME,
                                               NEGATIVE_CLASS_NAME)),
               y = factor(Prediction, level = c(NEGATIVE_CLASS_NAME,
                                                POSITIVE_CLASS_NAME)))) +
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


### Plot AUC
plot_auc <- function(pred.prob, references, method, class){
    pred.to.roc = pred.prob[, class]
    ##Plot
    pred.rocr = prediction(pred.to.roc, references)
    perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff") 
    perf.tpr.rocr = performance(pred.rocr, "tpr","fpr") 
    opt_cut = opt.cut(perf.tpr.rocr, pred.rocr)
    ## ROC curve
    filename = paste("images/", method, "_", class, "_auc.png", sep="")
    png(filename)
    plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
    abline(a=0, b=1)
    dev.off()
    ## Best cutoff
    acc.perf = performance(pred.rocr, measure = "acc")
    filename = paste("images/", method, "_", class, "_cutoff_auc.png", sep="")
    png(filename)
    plot(acc.perf, main = "Accurancy to changes in cutoff")
    dev.off()
    
    return(opt_cut)
}

### Optimal cutoff
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}

### Resolve labels
resolve_label <- function(df, probs, references){
    label_is_positive <-
        df[rownames(probs)[1], "award"] == POSITIVE_CLASS_NAME
    
    if (label_is_positive & references[1] == "1"){
        factor_1 = POSITIVE_CLASS_NAME
        factor_2 = NEGATIVE_CLASS_NAME
    } else{
        factor_1 = NEGATIVE_CLASS_NAME
        factor_2 = POSITIVE_CLASS_NAME
    }
    
    references_resolved = recode_factor(
        references, "1" = factor_1, "2" = factor_2)
    
    return(references_resolved)
}


compare_statistics <- function (dataframe, methods_list, tune_grid_list,
                                seed, n_folds, repeats, num_cores) {
    set.seed(SEED)

    control <- trainControl(method = "repeatedcv", number = n_folds,
                            repeats = repeats, classProbs = TRUE,
                            summaryFunction = twoClassSummary,
                            verboseIter = TRUE, allowParallel = TRUE)

    trained_models = list()
    
    ## Training
    filename = paste("models_comparison.RData", sep="")
    if (TRAIN_MODEL){      
        
        ## Start cluster
        cl <- makePSOCKcluster(num_cores)
        registerDoParallel(cl)
        
        ## Start training
        for (i in 1:length(METHODS_LIST)){
            method = METHODS_LIST[[i]]
            tune_grid = TUNE_GRID_LIST[[i]]
            
            print(paste(">>>", method))
            trained <- train(award ~ .,
                             data = dataframe,
                             method = method,
                             tuneGrid = tune_grid,
                             metric = "ROC",
                             trControl = control) 
            # Append the new model
            trained_models[[method]] = trained
        }
        
        ## Stop cluster
        stopCluster(cl)
        
        if (DUMP_MODEL){
            ## Save report
            save(trained_models, file=filename)
        }
    }else{
        load(filename)
    }
    
    ## Statistics
    cv.values = resamples(trained_models)
    summary(cv.values)

    png("images/compare_dot_plot.png",
        res = RES, width = 20, height = 20, units = "cm")
    dotplot(cv.values, metric = "ROC")
    dev.off()

    png("images/compare_bw_plot.png",
        res = RES, width = 40, height = 20, units = "cm")
    bwplot(cv.values, layout = c(3, 1))
    dev.off()

    png("images/compare_splom_plot.png",
        res = RES, width = 20, height = 20, units = "cm")
    splom(cv.values, metric = "ROC")
    dev.off()
    
    print(cv.values$timings)

    return (trained_models)
}

