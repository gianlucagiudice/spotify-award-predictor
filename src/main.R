# ------- Load custom functions -------
INSTALL_LIBRARIES = FALSE
DUMP_MODEL = FALSE
TRAIN_MODEL = FALSE
source('src/functions/preprocessing.R')
source('src/functions/training_svm.R')
source('src/functions/common_functions.R')
source('src/functions/decision_tree.R')

# ------------- Constants --------------
DPI <- 300
SCALE = 0.75

TERM_FREQUENCY_THLD <- 2 # Term frequency
YEAR_THLD <- 2005
POPULARITY_THLD <- 25
SEED = 830694 + 829664
NPC <- 7 # Number of principal components
N_FOLDS = 10 # Cross validation parameter


# ------------ Preprocessing ------------
# Read the dataset 
data = read_dataset("data/songs.csv")
print(head(data))
# Plot data
data_visualization(data, POPULARITY_THLD, YEAR_THLD)
# Songs after 2005 and minimum popularity
data <- subset(data, year >= YEAR_THLD & popularity > POPULARITY_THLD)
# Build a balanced dataset
data.balanced <- build_balanced_dataset(data, SEED)

# Feature visualization
df = build_dataframe(data.balanced)
df.numeric = df[[2]]
df.categorical = df[[3]]
df.award = df[[4]]
df = df[[1]]

# Principal components analysis
df.principal_components = perform_pca(df.numeric, NPC)
print(head(df.principal_components))

# Plot categorical feature
plot_categorical_feature(df)

# Bag of words representation for artists
artists.tf_thld <- build_term_frequency_matrix(df)

# Build dataframe for training
df.out = build_out_dataframe(
  artists.tf_thld, df.principal_components, df.categorical, df.award)
# Out dataframe
print(paste("Dimension of the dataset for training (rows x columns):",
            dim(df.out)[1], dim(df.out)[2]))


# ------------ Training ------------
set.seed(SEED)
folds = createFolds(df.out$award, k = N_FOLDS)

# ==== SVM ====
# Train model
if (TRAIN_MODEL){
    training_report = train_svm_cv(df.out, folds)
### usando la generica funzione di cross validation:
### training_report = cross_validation_generic(df.out, train_svm, list(COST_LIST,GAMMA_LIST), folds, "SVM")
    
    #expand.grid(C = cost_list, sigma = gamma_list)
    #method = "svmRadial"
    #ßtraining_report = cross_validation(df.out, "svmRadial", SEED, N_FOLDS, NUM_CORES)
    
    if (DUMP_MODEL){
        # Save report
        save(training_report, file="svmReport.RData")
    }
}else{
  load("svmReport.RData")
}
performance.positive_folds = training_report[[1]]
performance.negative_folds = training_report[[2]]
# Class performance
performance.positive = combine_folds_performance(performance.positive_folds)
performance.negative = combine_folds_performance(performance.negative_folds)
plot_class_performance(performance.positive[5:7], performance.negative[5:7])
# Confusion matrix
confusion_matrix = combine_folds_cm(performance.positive_folds)
plot_performance(confusion_matrix, performance.positive, performance.negative)
plot_cm(confusion_matrix)
# AUC
opt_cut = plot_auc(df.out, 'radial')
print("Optimal cutoff:")
print(opt_cut)


#  ==== Decision Tree ====


### In R in generale é meglio non avere spazi dentro al nome di una colonna, rpart senza prima normalizzare i nomi di colonna non funziona
### TODO questa operazione si potrebbe fare durante il preprocessing dei dati, tanto é veloce e non cambia niente per svm (penso)


decision_tree.training_report = cross_validation_generic(dataframe = df.out,
                                                         training_function = train_decision_tree,
                                                         fold_indexes = folds,
                                                         technique_name_string = "Decision Tree")

### TODO tutte le analisi del caso

decision_tree.performance.positive_folds = decision_tree.training_report[[1]]
decision_tree.performance.negative_folds = decision_tree.training_report[[2]]

## Class performance
decision_tree.performance.positive = combine_folds_performance(decision_tree.performance.positive_folds)
decision_tree.performance.negative = combine_folds_performance(decision_tree.performance.negative_folds)

plot_class_performance_generic(decision_tree.performance.positive[5:7], decision_tree.performance.negative[5:7], "decision_tree_class_performance.png")

## Confusion matrix
decision_tree.performance.confusion_matrix = combine_folds_cm(decision_tree.performance.positive_folds)
plot_performance_generic(decision_tree.performance.confusion_matrix, decision_tree.performance.positive, decision_tree.performance.negative, "decision_tree_performance.png")
plot_cm_generic(decision_tree.performance.confusion_matrix, "decision_tree_confusion_matrix.png")

### ------- Model comparison -------
### TODO 
