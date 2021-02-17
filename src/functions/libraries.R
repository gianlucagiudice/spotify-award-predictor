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

### --------- Libraries ---------
libraries = c("e1071", "caret", "ROCR", "C50", "pROC", "parallel",
              "libcoin", "kernlab", "doParallel")

if (INSTALL_LIBRARIES){
    install.packages(libraries, dependencies = TRUE, character.only = TRUE)
}
for (library in libraries){
    library(library, character.only = TRUE)
}
