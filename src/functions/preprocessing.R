# --------- Libraries ---------
libraries = c("plyr", "ggplot2", "readr", "caret", "dplyr", "GGally", "tidyverse",
              "ggcorrplot", "FactoMineR", "factoextra", "tm", "ggwordcloud",
              "wordcloud2", "webshot", "htmlwidgets", "dplyr", "data.table",
              "slam")
if (INSTALL_LIBRARIES){
  install.packages(libraries, character.only = TRUE)
}
for (library in libraries){
  library(library, character.only = TRUE)
}
webshot::install_phantomjs()


# --------- Functions ---------
# Read the dataset
read_dataset <- function(path) {
    # Read the dataset
    data <- read_csv("data/songs.csv")
    # Drop not useful information
    data = subset(data, select=c(2:10, 12:17, 19:20, 22))
    # Rename columns
    data <- rename(data, c("award"="award_type"))
    # Change each column to correct datatype
    data <- transform(data, valence=as.numeric(valence))
    data$mode <- factor(data$mode)
    data$key <- factor(data$key)
    data$explicit <- factor(data$explicit)
    data$mode <- factor(data$mode)
    data$award <- factor(!data$award == "null")
    # Convert to lowercase
    data$name = to_lower(data$name)
    data$artists = to_lower(data$artists)
    
    return(data)
}

to_lower <- function(input_list){
  i = 1
  result = rep(NA, length(input_list))
  lower = lapply(input_list, tolower)
  for (i in 1:length(input_list)){
    result[i] = lower[[i]]
  }
  return(result)
}

# Data exploartion
data_visualization <- function(data, popularity_thld) {
    ggplot(data, aes(year)) +
        ggtitle("Years distribution") +
        xlab("Year") + ylab("Num. of songs") + 
        geom_histogram(color="black", fill="white", binwidth = 1) +
        scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
        theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
    ggsave("years_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    ggplot(data, aes(y=year)) + 
        ggtitle("Boxplot year") +
        geom_boxplot() +
        scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("years_boxplot.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    # Popularity
    ggplot(data, aes(popularity)) +
        ggtitle("Popularity distribution") +
        xlab("Popularity") + ylab("Num. of songs") + 
        geom_histogram(color="black", fill="white", binwidth = 1) +
        scale_x_continuous(breaks = c(0:100)) +
        theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
    ggsave("popularity_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    ggplot(data, aes(y=popularity)) + 
        ggtitle("Boxplot popularity") +
        geom_boxplot() +
        scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("popularity_boxplot.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    # Minimum popularity
    # Assume we are interested in songs not completely unknown
    print(c("Popularity quantile:", quantile(data$popularity)))
    data <- subset(data, popularity > popularity_thld)

    ggplot(data, aes(y=year, x=award)) + 
        ggtitle("Boxplot year award vs not award") +
        xlab("Award won") + ylab("Year") + 
        geom_boxplot() +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("year_award_comparison.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    # Award vs no award
    ggplot(data, aes(award)) +
        ggtitle("Awards distribution") +
        xlab("Award") + ylab("Num. of songs") + 
        geom_bar(color="black", fill="white") +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("awards_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)
}

# Build a balanced dataset
build_balanced_dataset <- function(data, seed) {
    set.seed(seed)
    positive <- subset(data, award == TRUE)
    negative <- subset(data, award == FALSE)
    negative_sample <- negative[sample(nrow(negative), nrow(positive)), ]
    data_balanced = union(positive, negative_sample)

    return(data_balanced)
}

# Build dataframe
build_dataframe <- function(balanced) {
    # Keep only numeric features
    df = balanced
    df.numeric = subset(df, select=c(3, 5:8, 10, 12, 13, 15:17))
    df.award = df$award
    df.categorical = subset(df, select=c(9, 11, 14))
    # Standardize
    df.numeric = as.data.frame(scale(df.numeric, center = TRUE, scale = TRUE))
    # Sample 500 random points for plot purpose
    df.sample <- sample_n(df.numeric, 500, seed=seed)
    df.sample$award <- df[as.integer(rownames(df.sample)),]$award
    # Pair plot
    pairplot = ggpairs(df.sample, aes(colour = award, alpha = 0.2))
    pairplot
    ggsave("pairplot.png", plot = pairplot, path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
    # Remove useless feature (duration_ms)
    df.active = subset(df.numeric, select = c(1:3, 5:10))
    corr <- cor(df.active)
    ggcorrplot(corr)
    ggsave("correlation.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)

    to_return = list(df, df.numeric, df.award, df.categorical, df.numeric, df.sample, df.active)
    return(to_return)
}

# Principal component analysis
perform_pca <- function(active, ncp) {
    pca = PCA(active, scale.unit = TRUE, ncp = ncp)
    p <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) + 
        labs(title = "Variance explained + cumulative variance")
    # Cumulative variance
    cum_var = data.frame(x=1:length(pca$eig[, 3]), y=pca$eig[, 3])
    print("PCA Cumulative variance:")
    print(cum_var)
    p <- p +
    geom_point(data=cum_var, aes(x, y)) + 
    geom_line(data=cum_var, aes(x, y), color="red"); p
    ggsave("pca_variance_explained.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
    # Color by cos2 values: quality on the factor map
    fviz_pca_var(pca, col.var = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                repel = TRUE)
    ggsave("pca_dimensions_repr.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
    # Feature contribution
    fviz_contrib(pca, choice="var", axes = 1:ncp)
    ggsave("pca_feature_contribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
    # Projected data points over the 6 principal components
    df.pc6 = pca$ind$coord
    
    return(df.pc6)
}

# Categorical feature
plot_categorical_feature <- function(df) {
    # Explicit feature
    ggplot(data=df, aes(explicit, fill=factor(award))) +
    ggtitle("Explicit distribution") +
    geom_bar(colour="black", position="dodge") +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave("explicit_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)

    # Mode feature
    ggplot(data=df, aes(mode, fill=factor(award))) +
    ggtitle("Mode distribution") +
    geom_bar(colour="black", position="dodge") +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave("mode_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)

    # Key feature
    ggplot(data=df, aes(key, fill=factor(award))) +
    ggtitle("Key distribution") +
    geom_bar(colour="black", position="dodge") +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave("key_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
}

# Bag of words representation for artists
build_term_frequency_matrix <- function(df) {
    artists.df = data.frame(artists = df$artists)
    artists.df$award = df$award
    artists.tf =
        artists.df %>% 
        rownames_to_column(var="row") %>% 
        mutate(artists=str_split(artists, ",")) %>% 
        unnest(cols = c(artists)) %>% 
        mutate(dummy=1) %>% 
        spread(artists, dummy, fill=0)
    artists.tf = subset(artists.tf, select = c(2:ncol(artists.tf)))
    
    artists.occurrence = data.frame(occurrence = colSums(artists.tf[-1]))
    # Artists frequency
    ggplot(artists.occurrence, aes(occurrence)) +
        ggtitle("Frequency of occurrences of artists among songs") +
        xlab("Occurrence in songs") + ylab("Frequency") + 
        geom_histogram(color="black", fill="white", binwidth = 1) +
        scale_x_continuous(breaks = c(0:100)) +
        theme(axis.text.x = element_text(angle = 90),
                plot.title = element_text(hjust = 0.5))
    ggsave("artists_occurence.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)
    print(c("Occurrence quantile:", quantile(artists.occurrence$occurrence)))
    # Threshold over occurrence
    artists.occurrence_thld = subset(artists.occurrence,
                                    artists.occurrence$occurrence >= TERM_FREQUENCY_THLD)
    ggplot(artists.occurrence_thld, aes(occurrence)) +
    ggtitle(
        paste("Frequency of occurrences of artists among songs ( ccurrence >=",
            TERM_FREQUENCY_THLD, ")")) +
        xlab("Occurrence in songs") + ylab("Frequency") + 
        geom_histogram(color="black", fill="white", binwidth = 1) +
        scale_x_continuous(breaks = c(0:100)) +
        theme(axis.text.x = element_text(angle = 90),
                plot.title = element_text(hjust = 0.5))
    ggsave("artists_occurence_thld.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)
    print(c("Occurrence quantile:", quantile(artists.occurrence_thld$occurrence)))
    
    # Wordcloud overall
    plot_wordcloud(artists.occurrence_thld, "images/wordcloud_overall.png")
    
    # Wordcloud award
    artists.tf_positive = subset(artists.tf,
                                 artists.tf$award == "TRUE")
    artists.occurrence_positive = data.frame(
      occurrence = colSums(artists.tf_positive[-1]))
    
    artists.occurrence_thld_positive =
      subset(artists.occurrence_positive,
             artists.occurrence_positive$occurrence >= TERM_FREQUENCY_THLD)
    plot_wordcloud(artists.occurrence_thld_positive,
                   "images/wordcloud_positive.png")
    
    # Wordcloud not award
    artists.tf_negative = subset(artists.tf,
                                 artists.tf$award == "FALSE")
    artists.occurrence_negative = data.frame(
      occurrence = colSums(artists.tf_negative[-1]))
    
    artists.occurrence_thld_negative =
      subset(artists.occurrence_negative,
             artists.occurrence_negative$occurrence >= TERM_FREQUENCY_THLD)
    plot_wordcloud(artists.occurrence_thld_negative,
                   "images/wordcloud_negative.png")
    
    
    
    return(artists.tf[rownames(artists.occurrence_thld)])
}

plot_wordcloud <- function(df, image_name){
  wordcloud_df = df
  wordcloud_df$word = rownames(wordcloud_df)
  wordcloud_df$frew = wordcloud_df$occurrence
  wordcloud_df = subset(wordcloud_df, select = c(2,3))
  wordcloud_graph <- wordcloud2(wordcloud_df)
  wordcloud_graph
  saveWidget(wordcloud_graph, "/tmp/tmp.html", selfcontained = F)
  webshot("/tmp/tmp.html", image_name, delay=8, vwidth = 2000, vheight=2000)
    
}

# Build dataframe for training
build_out_dataframe <- function(df, tf, pc6, categorical, award){
    # Term frequency
    df.out = tf
    # Principal components
    df.out$pc1 = pc6[,1]
    df.out$pc2 = pc6[,2]
    df.out$pc3 = pc6[,3]
    df.out$pc4 = pc6[,4]
    df.out$pc5 = pc6[,5]
    df.out$pc6 = pc6[,6]    
    # Categorical features
    df.out$explicit = as.integer(categorical$explicit) - 1
    df.out$mode = as.integer(categorical$mode) - 1
    df.out$key = range01(as.integer(categorical$key) - 1)
    # Timestamp in milliseconds
    df.out$duration_ms = df$duration_ms
    # Label
    df.out$award = award

    return(df.out)
}