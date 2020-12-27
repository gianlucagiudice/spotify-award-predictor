# ---------- Libraries ----------
libraries = c("plyr", "ggplot2", "readr", "caret", "dplyr", "GGally",
              "ggcorrplot", "FactoMineR", "factoextra")
#install.packages(c("FactoMineR", "factoextra"))
if (FALSE){
  install.packages(libraries)
}
for (library in libraries){
  library(library, character.only = TRUE)
}

DPI = 300


# -------------------------------
# Read the dataset
data <- read_csv("data/songs.csv")

# Data preparation
print(data)
# Drop not useful information
data = subset(data, select=c(2:10, 12:17, 19:20, 22))
print(data)

# Rename columns
data <- rename(data, c("award"="award_type"))
# Change each column to correct datatype
data <- transform(data, valence=as.numeric(valence))
data$mode <- factor(data$mode)
data$key <- factor(data$key)
data$explicit <- factor(data$explicit)
data$mode <- factor(data$mode)
data$award <- factor(!data$award == "null")
head(data)

# Songs after 2000
data <- subset(data, year>2000)


# ---------- Dataset Exploration ----------
# Years©just = 0.5))
ggsave("years_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)

ggplot(data, aes(y=year)) + 
  ggtitle("Boxplot year") +
  geom_boxplot() +
  scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("years_boxplot.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)

# Popularity
ggplot(data, aes(popularity)) +
  ggtitle("Popularity distribution") +
  xlab("Popularity") + ylab("Num. of songs") + 
  geom_histogram(color="black", fill="white", binwidth = 1) +
  scale_x_continuous(breaks = c(0:100)) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
ggsave("popularity_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)

ggplot(data, aes(y=popularity)) + 
  ggtitle("Boxplot popularity") +
  geom_boxplot() +
  scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("popularity_boxplot.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)

# Minimum popularity
# Assume we are interested in songs not completely unknown
print(c("Popularity quantile:", quantile(data$popularity)))
data <- subset(data, popularity>25)

ggplot(data, aes(y=year, x=award)) + 
  ggtitle("Boxplot year award vs not award") +
  xlab("Award won") + ylab("Year") + 
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("year_award_comparison.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)

# Award vs no award
ggplot(data, aes(award)) +
  ggtitle("Awards distribution") +
  xlab("Award") + ylab("Num. of songs") + 
  geom_bar(color="black", fill="white") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("awards_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = DPI, limitsize = TRUE)


# ------ Consider a balanced dataset -------
seed = 830694 + 829664
positive <- subset(data, award == TRUE)
negative <- subset(data, award == FALSE)
negative_sample <- sample_n(data, nrow(positive), seed=seed)
data_balanced = union(positive, negative_sample)


# --------- Feature Visualization ----------
# Keep only numeric features
df = data_balanced
df.numeric = subset(df, select=c(3, 5:8, 10, 12, 13, 15:17))
df.award = df$award
df.categorical = subset(df, select=c(9, 11, 14))
# Standardize
df.numeric = as.data.frame(scale(df.numeric, center = TRUE, scale = TRUE))
head(df.numeric)
# Sample 500 random points for plot purpose
df.sample <- sample_n(df.numeric, 500, seed=seed)
df.sample$award <- df[as.integer(rownames(df.sample)),]$award
# Pair plot
ggpairs(df.sample, aes(colour = award, alpha = 0.2))
ggsave("pairplot.png", plot = last_plot(), path = "images",
       scale = 1.75, dpi = floor(DPI), limitsize = TRUE)
# Remove useless feature (duration_ms)
df.active = subset(df.numeric, select = c(1:3, 5:10))
corr <- cor(df.active)
ggcorrplot(corr)
ggsave("correlation.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)


# ----- Principal component analysis ------
pca = PCA(df.active, scale.unit = TRUE, ncp = 6)
p <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) + 
    labs(title = "Variance explained + cumulative variance")
# Cumulative variance
cum_var = data.frame(x=1:length(pca$eig[, 3]), y=pca$eig[, 3])
print("Cumulative variance:")
print(cum_var)
p <- p +
  geom_point(data=cum_var, aes(x, y)) + 
  geom_line(data=cum_var, aes(x, y), color="red"); p
ggsave("pca_variance_explained.png", plot = last_plot(), path = "images",
      scale = 1, dpi = floor(DPI), limitsize = TRUE)
# Color by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
ggsave("pca_dimensions_repr.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)
# Feature contribution
fviz_contrib(pca, choice="var", axes = 1:6)
ggsave("pca_feature_contribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)
# Projected data points over the 6 principal components
df.pc6 = pca$ind$coord
print(head(df.pc6))


# -------- Categorical features ---------
# Explicit feature
ggplot(data=df, aes(explicit, fill=factor(award))) +
  ggtitle("Explicit distribution") +
  geom_bar(colour="black", position="dodge") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("explicit_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)

# Mode feature
ggplot(data=df, aes(mode, fill=factor(award))) +
  ggtitle("Mode distribution") +
  geom_bar(colour="black", position="dodge") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("mode_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)

# Key feature
ggplot(data=df, aes(key, fill=factor(award))) +
  ggtitle("Key distribution") +
  geom_bar(colour="black", position="dodge") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("key_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)


# ------ Dataframe for training -------
# Categorical features
df.out = df.categorical
df.out$explicit = as.integer(df.categorical$explicit) - 1
df.out$mode = as.integer(df.categorical$mode) - 1
df.out$key = rescale01(as.integer(df.categorical$key) - 1)
# Principal components
df.out$pc1 = df.pc6[,1]
df.out$pc2 = df.pc6[,2]
df.out$pc3 = df.pc6[,3]
df.out$pc4 = df.pc6[,4]
df.out$pc5 = df.pc6[,5]
df.out$pc6 = df.pc6[,6]
# Timestamp in ms
df.out$duration_ms = df.numeric$duration_ms
# Out dataframe
print(head(df.out))
