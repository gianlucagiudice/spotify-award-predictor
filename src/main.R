# ---------- Libraries ----------
#install.packages(c("plyr", "ggplot2", "readr", "caret", "dplyr"))
library(plyr)
library(ggplot2)
library(readr)
library(caret)
library(dplyr)
library(cowplot)

DPI = 300


# -------------------------------
# Read the dataset
data <- read_csv("data/songs.csv")

# Data preparation
print(data)
# Drop not useful information
data = subset(data, select=c(2:10, 12:17, 19:20, 22))
# Rename columns
data <- rename(data, c("award_type"="award"))
# Change each column to correct datatype
data <- transform(data, valence=as.numeric(valence))
data$mode <- factor(data$mode)

# Add labels to each song
data$award <- !data$award == "null"
data$award <- factor(data$award)
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
negative_sample <- sample_n(data, nrow(positve), seed=seed)
data_balanced = union(positive, negative_sample)


# --------- Feature Visualization ----------
# Keep only features
df = subset(data_balanced, select=c(3:18))

if(FALSE){
x <- sample_n(df, 200, seed=seed)
featurePlot(x = x[, 1:16], y = x$award, plot="pairs",
            scales=list(x=list(relation="free"), y=list(relation="free")),
            auto.key=list(columns=3))
            

featurePlot(x = df[, 1:16], y = df$award, plot = "pairs")


featurePlot(x = df[,1:3], y = df$award, plot = "pairs")



my_plots <- lapply(names(x), function(var_x){
  p <- 
    ggplot(x) +
    aes_string(var_x)
  
  if(is.numeric(iris[[var_x]])) {
    p <- p + geom_histogram() +
      scale_x_continuous(breaks = c(min(var_x):max(var_x)))
    
  } else {
    p <- p + geom_bar()
  } 
  
})
plot_grid(plotlist = my_plots)
}