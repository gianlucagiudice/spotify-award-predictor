libraries = c("ggplot2", "readr", "GGally", "ggcorrplot", "FactoMineR",
              "wordcloud2", "webshot", "slam", "tidyr", "stringr",
              "dplyr", "caret", "ROCR", "doParallel")

if (INSTALL_LIBRARIES){
  installed_libraries = rownames(installed.packages())
  
  for (library in libraries){
    if (! (library %in% installed_libraries)){
      install.packages(library, dependencies = TRUE, character.only = TRUE)
    }
  }
}

for (library in libraries){
  library(library, character.only = TRUE)
}
