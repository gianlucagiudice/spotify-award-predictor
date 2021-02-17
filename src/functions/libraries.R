libraries = c("ggplot2", "readr", "GGally", "ggcorrplot", "FactoMineR",
              "wordcloud2", "webshot", "slam", "tidyr", "stringr",
              "dplyr", "caret", "ROCR", "doParallel")

if (INSTALL_LIBRARIES){
  install.packages(libraries, dependencies = TRUE, character.only = TRUE)
}
for (library in libraries){
  library(library, character.only = TRUE)
}
