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
data$award <- !data$award == "null"
head(data)

# Songs after 2000
data <- subset(data, year>2000)


# ---------- Dataset Exploration ----------
# Years
ggplot(data, aes(year)) +
  ggtitle("Years distribution") +
  xlab("Year") + ylab("Num. of songs") + 
  geom_histogram(color="black", fill="white", binwidth = 1) +
  scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
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
df = subset(data_balanced, select=c(3, 5:8, 10, 12, 13, 15:18))
# Standardize
X = as.data.frame(scale(df[1:ncol(df)-1], center = TRUE, scale = TRUE))
y = df$award
df = X
df$award = y
head(df)
# Sample 500 random points for plot purpose
df.sample <- sample_n(df, 500, seed=seed)
ggpairs(df.sample, aes(colour = award, alpha = 0.2))
ggsave("pairplot.png", plot = last_plot(), path = "images",
       scale = 1.75, dpi = floor(DPI), limitsize = TRUE)
# Remove useless feature
df.effective = subset(df, select = c(1:5, 7:11))
df.award = df$award
corr <- cor(df.effective)
ggcorrplot(corr)
ggsave("correlation.png", plot = last_plot(), path = "images",
       scale = 1, dpi = floor(DPI), limitsize = TRUE)


# ----- Principal component analysis ------
PCA(df.effective, scale.unit = TRUE, graph = TRUE)
