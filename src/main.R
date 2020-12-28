# ------- Load custom functions -------
source('src/functions/preprocessing.R')
source('src/functions/training_svm.R')
source('src/functions/training_decisiontree.R')


# ------------- Constants --------------
DPI <- 300
TERM_FREQUENCY_THLD <- 3
YEAR_THLD <- 2005
POPULARITY_THLD <- 25
SEED = 830694 + 829664


# ------------ Preprocessing ------------
# Read the dataset 
data = read_dataset("data/songs.csv")
print(head(data))
# Songs after 2000
data <- subset(data, year >= YEAR_THLD)

# Plot data
data_visualization(data, POPULARITY_THLD)
# Assume we are interested in songs not completely unknown (Minimum popularity)
data <- subset(data, popularity > POPULARITY_THLD)

# Build a balanced dataset
data.balanced <- build_balanced_dataset(data, SEED)

# Feature visualization
df = build_dataframe(data.balanced)
df.numeric = df[[2]]
df.award = df[[3]]
df.categorical = df[[4]]
df.numeric = df[[5]]
df.sample = df[[6]]
df.active = df[[7]]
df = df[[1]]

# Principal components analysis
df.pc6 = perform_pca(df.active)
print(head(df.pc6))

# Plot categorical feature
plot_categorical_feature(df)

# Bag of words representation for artists
artists.tf_thld <- build_term_frequency_matrix(df)

# Build dataframe for training
df.out = build_out_dataframe(
  df.numeric, artists.tf_thld, df.pc6, df.categorical, df.award)
# Out dataframe
print(paste("Dimension of the dataset for training (rows x columns):",
            dim(df.out)[1], dim(df.out)[2]))


# ------------ Training SVM ------------
# TODO


# ------- Training Decision tree -------
# TODO
