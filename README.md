# Purpose of the Repo

This repo is dedicated to ST-558 Project 2, for Group 9 - [Maxwell Marion-Spencer](mailto:msmarion@ncsu.edu) and [Xin Wang](mailto:xwang239@ncsu.edu).

The focus of this project is on automating reports using Markdown, and building predictive models. The data used for this project is an online news popularity data set, from which we will build predictive models with the intention of predicting the number of shares. Each data channel will have it's own output, as the creation of Markdown files has been automated.

# List of R Packages Used (check?)

```
library(tidyverse)
library(tibble)
library(caret)
library(gbm)
library(knitr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(vtable)
```

# Links to the analyses (check?)

The analysis for [Bus](busAnalysis.md)\
The analysis for [Entertainment](entertainmentAnalysis.md)\
The analysis for [Lifestyle](lifestyleAnalysis.md)\
The analysis for [Socmed](socmeAnalysis.md)\
The analysis for [Tech](techAnalysis.md)\
The analysis for [World](worldAnalysis.md)

# Code Used to Create Analyses (check?)

```
data_channel <- c("bus","entertainment","lifestyle","socmed","tech","world")
output_file <- paste0(data_channel, "Analysis.md")
params = lapply(data_channel, 
                FUN = function(x){
                  list(data_channel = x)})
reports <- tibble(output_file, params)
apply(reports, 
      MARGIN = 1,
      FUN = function(x){
        rmarkdown::render(input = "st558 - project2.Rmd", 
                          output_format = "github_document", 
                          output_options = list(html_preview = FALSE), 
                          output_file = x[[1]], 
                          params = x[[2]])})
```
