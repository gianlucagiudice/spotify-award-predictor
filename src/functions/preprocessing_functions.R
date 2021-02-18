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
    # Label
    data$award <- factor(!data$award == "null")
    data$award <- recode_factor(
      data$award, "TRUE" = POSITIVE_CLASS_NAME, "FALSE" = NEGATIVE_CLASS_NAME)
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
data_visualization <- function(data_all, popularity_thld, year_yhld, plot_graphs) {
  if(!plot_graphs){
    return()
  }
  
    data = subset(data_all, year >= year_yhld)
    
    # Year distribution
    ggplot(data_all, aes(year)) +
      ggtitle("Years distribution") +
      xlab("Year") + ylab("Num. of songs") + 
      geom_histogram(color="black", fill="white", binwidth = 1) +
      scale_x_continuous(
        breaks = seq(min(data_all$year), max(data_all$year), by = 3)) +
      theme(axis.text.x = element_text(angle = 90),
            plot.title = element_text(hjust = 0.5))
    ggsave("years_distribution.png", plot = last_plot(), path = "images",
           scale = SCALE, dpi = DPI, limitsize = TRUE)
    
    # Year distribution - thld
    ggplot(data, aes(year)) +
        ggtitle("Years distribution") +
        xlab("Year") + ylab("Num. of songs") + 
        geom_histogram(color="black", fill="white", binwidth = 1) +
        scale_x_continuous(breaks = c(min(data$year):max(data$year))) +
        theme(axis.text.x = element_text(angle = 90),
              plot.title = element_text(hjust = 0.5))
    ggsave("years_distribution_thld.png", plot = last_plot(), path = "images",
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
        scale_x_continuous(breaks = seq(0, 100, by = 2)) +
        theme(axis.text.x = element_text(angle = 90),
              plot.title = element_text(hjust = 0.5))
    ggsave("popularity_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE, dpi = DPI, limitsize = TRUE)

    ggplot(data, aes(y=popularity)) + 
        ggtitle("Boxplot popularity") +
        ylab("Popularity") +
        geom_boxplot() +
        coord_flip() +
        scale_y_continuous(breaks = seq(0, 100, by = 5)) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_blank(), axis.ticks.y = element_blank())
    ggsave("popularity_boxplot.png", plot = last_plot(), path = "images",
           height=7, width=17, units="cm",
           scale = SCALE * 1.5, dpi = DPI, limitsize = TRUE)

    # Minimum popularity
    # Assume we are interested in songs not completely unknown
    print(c("Popularity quartile:", quantile(data$popularity)))
    data <- subset(data, popularity > popularity_thld)
    print(c("Popularity mean:", mean(mean(data$popularity))))
    print(c("Popularity std:", sqrt(var(data$popularity))))

    ggplot(data, aes(y=year, x=award)) + 
        ggtitle("Boxplot year award vs not award") +
        xlab("Award won") + ylab("Year") + 
        geom_boxplot() +
        coord_flip() +
        scale_y_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("year_award_comparison.png", plot = last_plot(), path = "images",
           height=15, width=30, units="cm",
           scale = SCALE, dpi = DPI, limitsize = TRUE)

    # Award vs no award
    ggplot(data, aes(award)) +
        ggtitle("Awards distribution") +
        xlab("Award") + ylab("Num. of songs") + 
        geom_bar(color="black", fill="white") +
        coord_flip() +
        scale_y_continuous(breaks = seq(min(0), max(30000), by = 1000)) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90))
    ggsave("awards_distribution.png", plot = last_plot(), path = "images",
           height=15, width=40, units="cm",
           scale = SCALE / 1.25, dpi = DPI, limitsize = TRUE)
}

# Build a balanced dataset
build_balanced_dataset <- function(data, seed) {
    set.seed(seed)
    positive <- subset(data, award == POSITIVE_CLASS_NAME)
    negative <- subset(data, award == NEGATIVE_CLASS_NAME)
    negative_sample <- negative[sample(nrow(negative), nrow(positive)), ]
    data_balanced = union(positive, negative_sample)

    return(data_balanced)
}

### Build dataframe
build_dataframe <- function(balanced, plot_graph) {
    ## Keep only numeric features
    df = balanced
    df.numeric = subset(df, select=c(3, 5:8, 10, 12, 13, 15:17))
    df.award = df$award
    df.categorical = subset(df, select=c(9, 11, 14))
    ## Standardize
    df.numeric = as.data.frame(scale(df.numeric, center = TRUE, scale = TRUE))
    ## Sample 500 random points for plot purpose
    if (plot_graph) {
        SAMPLE_SIZE = 500
        df.sample = df.numeric
        df.sample$award = df.award
        df.sample <- union_all(
            sample_n(subset(df.sample, award == POSITIVE_CLASS_NAME),
                     SAMPLE_SIZE / 2, seed=SEED),
            sample_n(subset(df.sample, award == NEGATIVE_CLASS_NAME),
                     SAMPLE_SIZE / 2, seed=SEED))

        ## Pair plot
        pairplot = ggpairs(df.sample, aes(colour = award, alpha = 0.15))

        ggsave("pairplot.png", plot = pairplot, path = "images",
               scale = SCALE * 1.5, dpi = DPI, 
               height=29, width=40, units="cm",
               limitsize = FALSE)

        ## Correlation plot
        corr <- cor(df.numeric)
        ggcorrplot(corr)
        ggsave("correlation.png", plot = last_plot(), path = "images",
               scale = SCALE, dpi = floor(DPI), limitsize = TRUE)
    }
    
    
    ## Remove popularity feature for trainig purpose
    df.numeric_used = subset(df, select=c(3, 5:8, 10, 12, 13, 15:16))
    to_return = list(df, df.numeric_used, df.categorical, df.award)
    return(to_return)
}

### Principal component analysis
perform_pca <- function(active, ncp, plot_graph) {
    pca = PCA(active, scale.unit = TRUE, ncp = ncp, graph = plot_graph)
    print("PCA results:")
    print(pca$eig)

    if (plot_graph) {

        ## Pca explained variance plot
        pca_df = as.data.frame.matrix(pca$eig)
        pca_df$id = 1:nrow(pca_df)
        
        ggplot(data = pca_df) +
            ggtitle("Explained variance + cumulative variance") +
            xlab("Principal component") + ylab("Explained variance") +
            geom_col(aes(x = id, y = pca_df[,2]), color = "black", fill = "white") +
            geom_line(aes(x = id, y = pca_df[,3]), color="red") +
            geom_point(aes(x = id, y = pca_df[,3]), color="red") +
            scale_y_continuous(breaks = seq(0, 100, by = 5)) +
            scale_x_continuous(breaks = seq(0, nrow(pca_df), by = 1)) +
            theme(plot.title = element_text(hjust = 0.5))
        ggsave("pca_variance_explained.png", plot = last_plot(), path = "images",
               scale = SCALE / 1.25, dpi = DPI, limitsize = TRUE)

        ## PCA contribution
        fviz_pca_var(pca, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                     repel = TRUE)
        ggsave("pca_dimensions_repr.png", plot = last_plot(), path = "images",
               scale = SCALE, dpi = floor(DPI), limitsize = TRUE)

        ## Feature contribution
        fviz_contrib(pca, choice="var", axes = 1:ncp)
        ggsave("pca_feature_contribution.png", plot = last_plot(), path = "images",
               scale = SCALE, dpi = floor(DPI), limitsize = TRUE)

    }
    ## Projected data points over the 7 principal components
    df.principal_components = pca$ind$coord
    
    return(df.principal_components)
}

## Categorical feature
plot_categorical_feature <- function(df, plot_graph) {
    if (!plot_graph){
        return();
    }
    ## Explicit feature
    ggplot(data=df, aes(explicit, fill=factor(award))) +
        ggtitle("Explicit distribution") +
        geom_bar(colour="black", position="dodge") +
        scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
        theme(plot.title = element_text(hjust = 0.5))
    ggsave("explicit_distribution.png", plot = last_plot(), path = "images",
           scale = SCALE / 2, dpi = floor(DPI), limitsize = TRUE,
           height=30, width=30, units="cm")

    ## Mode feature
    ggplot(data=df, aes(mode, fill=factor(award))) +
    ggtitle("Mode distribution") +
    geom_bar(colour="black", position="dodge") +
    scale_y_continuous(breaks = seq(0, 1300, by = 100)) +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave("mode_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE / 2, dpi = floor(DPI), limitsize = TRUE,
        height=30, width=30, units="cm")

    ## Key feature
    ggplot(data=df, aes(key, fill=factor(award))) +
    ggtitle("Key distribution") +
    geom_bar(colour="black", position="dodge") +
    scale_y_continuous(breaks = seq(0, 300, by = 20)) +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave("key_distribution.png", plot = last_plot(), path = "images",
        scale = SCALE / 2, dpi = floor(DPI), limitsize = TRUE,
        height=30, width=50, units="cm")
}

### Hot encoding representation for artists
build_term_frequency_matrix <- function(df, plot_graph) {
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

    if (plot_graph) {

        ## Artists frequency
        ggplot(artists.occurrence, aes(occurrence)) +
            ggtitle("Frequency of occurrences of artists among songs") +
            xlab("Occurrence in songs") + ylab("Frequency") + 
            geom_histogram(color="black", fill="white", binwidth = 1) +
            scale_x_continuous(breaks = seq(0, 100, by = 1)) +
            scale_y_continuous(breaks = seq(0, 1500, by = 150)) +
            theme(axis.text.x = element_text(angle = 90),
                  plot.title = element_text(hjust = 0.5))
        ggsave("artists_occurence.png", plot = last_plot(), path = "images",
               scale = SCALE / 1.5, dpi = DPI, limitsize = TRUE)
    }
    

    ## Threshold over occurrence
    artists.occurrence_thld =
      subset(artists.occurrence,
             artists.occurrence$occurrence >= TERM_FREQUENCY_THLD)

    if (plot_graph) {

        ggplot(artists.occurrence_thld, aes(occurrence)) +
            ggtitle(
                paste("Frequency of occurrences of artists among songs ( occurrence >=",
                      TERM_FREQUENCY_THLD, ")")) +
            xlab("Occurrence in songs") + ylab("Frequency") + 
            geom_histogram(color="black", fill="white", binwidth = 1) +
            scale_x_continuous(breaks = seq(0, 100, by = 1)) +
            scale_y_continuous(breaks = seq(0, 1500, by = 20)) +
            theme(axis.text.x = element_text(angle = 90),
                  plot.title = element_text(hjust = 0.5))
        ggsave("artists_occurence_thld.png", plot = last_plot(), path = "images",
               scale = SCALE, dpi = DPI, limitsize = TRUE)
        
        ## Wordcloud overall
        plot_wordcloud(artists.occurrence_thld, "images/wordcloud_overall.png")
        
        ## Wordcloud award
        artists.tf_positive = subset(artists.tf,
                                     artists.tf$award == POSITIVE_CLASS_NAME)
        artists.occurrence_positive = data.frame(
            occurrence = colSums(artists.tf_positive[-1]))
        
        artists.occurrence_thld_positive =
            subset(artists.occurrence_positive,
                   artists.occurrence_positive$occurrence >= TERM_FREQUENCY_THLD)
        plot_wordcloud(artists.occurrence_thld_positive,
                       "images/wordcloud_positive.png")
        
        ## Wordcloud not award
        artists.tf_negative = subset(artists.tf,
                                     artists.tf$award == NEGATIVE_CLASS_NAME)
        artists.occurrence_negative = data.frame(
            occurrence = colSums(artists.tf_negative[-1]))
        
        artists.occurrence_thld_negative = 
            subset(artists.occurrence_negative,
                   artists.occurrence_negative$occurrence >= TERM_FREQUENCY_THLD)
        plot_wordcloud(artists.occurrence_thld_negative,
                       "images/wordcloud_negative.png")
    }    
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
  webshot("/tmp/tmp.html", image_name, delay=15, vwidth = 2000, vheight=1250)
    
}

### Build dataframe for training
build_out_dataframe <- function(tf, principal_components, categorical, award){
    ## Term frequency
    df.out = tf
    ## Principal components
    df.out = cbind(df.out, principal_components)
    ## Categorical features
    df.out$explicit = as.integer(categorical$explicit) - 1
    df.out$mode = as.integer(categorical$mode) - 1
    df.out$key = range01(as.integer(categorical$key) - 1)
    ## Label
    df.out$award = award

    ## Normalize column names by removing spaces
    colnames(df.out) <- make.names(colnames(df.out))

    return(df.out)
}
