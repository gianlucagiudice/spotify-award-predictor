### ------- Load custom functions -------
INSTALL_LIBRARIES = FALSE
DUMP_MODEL = TRUE
TRAIN_MODEL = TRUE
PLOT_GRAPHS = TRUE

source('src/functions/preprocessing.R')
source('src/functions/common_functions.R')
source('src/functions/decision_tree.R')

### ------------- Constants --------------
DPI <- 300
SCALE = 0.75
RES = 1800

POSITIVE_CLASS_NAME = "award"
NEGATIVE_CLASS_NAME = "not_award"

TERM_FREQUENCY_THLD <- 2 # Term frequency
YEAR_THLD <- 2005
POPULARITY_THLD <- 25
SEED = 830694 + 829664
NPC <- 7 # Number of principal components

NUM_CORES = detectCores(logical = TRUE) # Number of cores to use

# Training parameters
N_FOLDS = 10 # Cross validation parameter
N_REPEATS = 3

COST_LIST = 10^(-3:1)
GAMMA_LIST = 10^(-5:-1)
COMPLEXITY_LIST = 10^(-2) * (1:10) * 1

METHODS_LIST = list("svmRadial", "svmLinear", "rpart")
TUNE_GRID_LIST = list(
    expand.grid(C = COST_LIST, sigma = GAMMA_LIST), ## svmRadial
    expand.grid(C = COST_LIST), ##svmlinear
    NULL) ## rpart


### ------------ Preprocessing ------------
## Read the dataset 
data = read_dataset("data/songs.csv")
print(head(data))
## Plot data
data_visualization(data, POPULARITY_THLD, YEAR_THLD, PLOT_GRAPHS)
## Songs after 2005 and minimum popularity
data <- subset(data, year >= YEAR_THLD & popularity > POPULARITY_THLD)
## Build a balanced dataset
data.balanced <- build_balanced_dataset(data, SEED)

## Feature visualization
df = build_dataframe(data.balanced, PLOT_GRAPHS)
df.numeric = df[[2]]
df.categorical = df[[3]]
df.award = df[[4]]
df = df[[1]]

## Principal components analysis
df.principal_components = perform_pca(df.numeric, NPC, PLOT_GRAPHS)
print(head(df.principal_components))

## Plot categorical feature
plot_categorical_feature(df, PLOT_GRAPHS)

## Bag of words representation for artists
artists.tf_thld <- build_term_frequency_matrix(df, PLOT_GRAPHS)

## Build dataframe for training
df.out = build_out_dataframe(artists.tf_thld,
                             df.principal_components,
                             df.categorical,
                             df.award)
## Out dataframe
print(paste("Dimension of the dataset for training (rows x columns):",
            dim(df.out)[1], dim(df.out)[2]))

dataframe = df.out

## ----------- DA RIMUOVERE -----------
df.reduced = subset(df.out, select = c(661, 662, 666))
df.reduced = union_all(df.reduced[1:150,], df.reduced[2500:(2500+150),])
colnames(df.reduced) <- make.names(colnames(df.reduced))
dataframe = df.reduced
## ----------- DA RIMUOVERE -----------


### ------------ Training ------------
print("=========== START MODELS TRAINING ===========")
for (i in 1:length(METHODS_LIST)){
  method = METHODS_LIST[[i]]
  tune_grid = TUNE_GRID_LIST[[i]]
  
  print(paste(">>>", method))
  train_target_model(dataframe = dataframe,
                     method = method,
                     tune_grid = tune_grid,
                     seed = SEED,
                     n_folds = N_FOLDS,
                     num_cores = NUM_CORES)
  print("------------")
}

### ------- Model comparison -------
print("=========== START COMPARISON ===========")
compare_statistics(dataframe = dataframe,
                   methods_list = METHODS_LIST,
                   tune_grid_list = TUNE_GRID_LIST,
                   seed = SEED,
                   n_folds = N_FOLDS,
                   repeats = N_REPEATS,
                   num_cores = NUM_CORES)

##compare_statistics(df.out, "rpart", "svmRadial", SEED, N_FOLDS, N_REPEATS)
