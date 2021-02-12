### TRAINING 

train_decision_tree <- function(training_data){

    trained_model = train(award ~ .,
                          data = training_data,
                          method = "rpart")
    return(trained_model)
}

### DECISION TREE SPECIFIC PLOTS


