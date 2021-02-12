### TRAINING 

train_decision_tree <- function(training_data){

    trained_model = train(award ~ .,
                          data = training_data,
                          method = "rpart")
    return(trained_model)
}

### DECISION TREE SPECIFIC PLOTS


train_svm <- function(training_data, cost_list, gamma_list) {
    svm.trained_cv = train(award ~ .,
                           data = training_data,
                           method = "svmRadial",
                           tuneGrid = expand.grid(
                             C = cost_list,
                             sigma = gamma_list
                           ),
                           num.threads = detectCores())
}


