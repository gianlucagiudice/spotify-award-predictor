# ------- Load custom functions -------
INSTALL_LIBRARIES = FALSE
DUMP_MODEL = FALSE
TRAIN_MODEL = FALSE
source('src/functions/preprocessing.R')
source('src/functions/training_svm.R')
source('src/functions/training_decisiontree.R')


# ------------- Constants --------------
DPI <- 300
SCALE = 0.75

TERM_FREQUENCY_THLD <- 2 # Term frequency
YEAR_THLD <- 2005
POPULARITY_THLD <- 25
SEED = 830694 + 829664
NPC <- 6 # Number of principal components
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
df.pc6 = perform_pca(df.numeric, NPC)
print(head(df.pc6))

# Plot categorical feature
plot_categorical_feature(df)

# Bag of words representation for artists
artists.tf_thld <- build_term_frequency_matrix(df)

# Build dataframe for training
df.out = build_out_dataframe(artists.tf_thld, df.pc6, df.categorical, df.award)
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
# TODO


# ------- Model comparison -------
# TODO
