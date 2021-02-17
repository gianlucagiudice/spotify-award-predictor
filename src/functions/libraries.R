# --------- Libraries ---------
libraries = c("plyr", "ggplot2", "readr", "caret", "dplyr", "GGally", "tidyverse",
              "ggcorrplot", "FactoMineR", "factoextra", "tm", "ggwordcloud",
              "wordcloud2", "webshot", "htmlwidgets", "dplyr", "data.table",
              "slam")


libraries = c("plyr", "ggplot2", "readr", "dplyr",
              "ggcorrplot", "tm", "FactoMineR",
              "wordcloud2", "webshot", "data.table",
              "slam", "tidyr", "tibble", "stringr")
if (INSTALL_LIBRARIES){
  install.packages(libraries, character.only = TRUE)
}
for (library in libraries){
  library(library, character.only = TRUE)
}
webshot::install_phantomjs()

### --------- Libraries ---------
libraries = c( "caret", "ROCR", "C50", "pROC", "parallel",
               "libcoin", "kernlab", "doParallel", "e1071")

libraries = c( "caret", "parallel", "ROCR",
               "doParallel")

if (INSTALL_LIBRARIES){
    install.packages(libraries, dependencies = TRUE, character.only = TRUE)
}
for (library in libraries){
    library(library, character.only = TRUE)
}
