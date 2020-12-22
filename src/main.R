# ---------- Libraires ----------
#install.packages(c("plyr", "ggplot2", "readr"))
library(plyr)
library(ggplot2)
library(readr)


# -------------------------------
# Read the dataset
df <- read_csv("data/songs.csv")

# Data preparation
print(df)
# Drop not useful information
df = subset(df, select=c(2:10, 12:20, 22))
# Rename columns
df <- rename(df, c("award_type"="award"))
# Change each column to correct datatype
df <- transform(df, valence=as.numeric(valence))
# Add labels to each song
df$award <- !df$award == "null"
df$award <- factor(df$award)
head(df)

# Explore the dataset
# TODO: Describe!

ggplot(df, aes(year)) +
  ggtitle("Years distribution") +
  xlab("Year") + ylab("Num. of songs") + 
  geom_histogram(color="black", fill="white", binwidth = 1) +
  scale_x_continuous(breaks = c(min(df$year):max(df$year))) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
ggsave("years_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = 300, limitsize = TRUE)

# Explore the dataset
ggplot(df, aes(popularity)) +
  ggtitle("Popularity distribution") +
  xlab("Popularity") + ylab("Num. of songs") + 
  geom_histogram(color="black", fill="white", binwidth = 1) +
  scale_x_continuous(breaks = c(0:100)) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
ggsave("popularity_distribution.png", plot = last_plot(), path = "images",
       scale = 1, dpi = 300, limitsize = TRUE)

# Songs after 2000
df <- subset(df, year>2000)
print(df)
# Assumiamo di voler rimuovere le canzoni completamenti sconoscite. Minori 2 volte la sd
