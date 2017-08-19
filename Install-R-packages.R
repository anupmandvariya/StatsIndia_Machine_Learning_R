## install R packages from CRAN
pkgs <- c("arules", "arulesSequences", "arulesViz", "caret", "cluster", "data.table", "dplyr", "e1071",
          "ff", "fpc", "foreign", "ggplot2", "glmnet", 
          "Hmisc", "igraph", "knitr", "lattice", "lda",
          "magrittr", "MASS", "party", 
          "randomForest", "RColorBrewer", "rgl", "rJava", "rmarkdown", "ROAuth", "ROCR", "RODBC", 
          "scatterplot3d", "sna", "SnowballC", "snowfall", "stringi", "stringr",
          "TH.data", "topicmodels", "tm", "twitteR", "ROAuth",
          "visNetwork", "wordcloud", "xlsx")

## exclude packages already installed
pkgs.installed <- installed.packages()
pkgs.to.install <- setdiff(pkgs, pkgs.installed[, 1])
print(pkgs.to.install)
install.packages(pkgs.to.install)

## install R packages from Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("Rgraphviz")