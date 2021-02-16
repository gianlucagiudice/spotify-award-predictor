# ------- Load custom functions -------
INSTALL_LIBRARIES = FALSE
DUMP_MODEL = FALSE
TRAIN_MODEL = TRUE
source('src/functions/preprocessing.R')
source('src/functions/training_svm.R')
source('src/functions/common_functions.R')
source('src/functions/decision_tree.R')

# ------------- Constants --------------
DPI <- 300
SCALE = 0.75

POSITIVE_CLASS_NAME = "award"
NEGATIVE_CLASS_NAME = "not_award"

TERM_FREQUENCY_THLD <- 2 # Term frequency
YEAR_THLD <- 2005
POPULARITY_THLD <- 25
SEED = 830694 + 829664
NPC <- 7 # Number of principal components
N_FOLDS = 10 # Cross validation parameter

NUM_CORES = detectCores(logical = TRUE) # Number of cores


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

dataframe = df.out

## ----------- DA RIMUOVERE -----------
df.reduced = subset(df.out, select = c(661, 662, 666))
df.reduced = union_all(df.reduced[1:500,], df.reduced[2500:(2500+500),])
colnames(df.reduced) <- make.names(colnames(df.reduced))
dataframe = df.reduced
## ----------- DA RIMUOVERE -----------


# ------------ Training ------------
# ==== SVM ====
method = "svmRadial"
tune_grid = expand.grid(C = COST_LIST, sigma = GAMMA_LIST)
train_target_model(dataframe = dataframe,
                   method = method,
                   tune_grid = tune_grid,
                   seed = SEED,
                   n_folds = N_FOLDS,
                   num_cores = NUM_CORES)

#  ==== SVM LINEAR ====
# Inserire qui

#  ==== DECISION TREE ====
method = "rpart"
tune_grid = expand.grid(cp = COPMLEXITY_LIST)
train_target_model(dataframe = dataframe,
                   method = method,
                   seed = SEED,
                   n_folds = N_FOLDS,
                   num_cores = NUM_CORES)


### ------- Model comparison -------
### TODO 

decision_tree <- funzione_roc(df.out, "rpart", "award", 2, 1, "green")
svm <- funzione_roc(df.out, "svmRadial", 2, 1, "red")
