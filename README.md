# Purpose of the Repo

heyy max, could you write this part please

# List of R Packages Used



# links to the analyses

The analysis for [Bus](busAnalysis.md)\
The analysis for [Entertainment](entertainmentAnalysis.md)\
The analysis for [Lifestyle](lifestyleAnalysis.md)\
The analysis for [Socmed](socmeAnalysis.md)\
The analysis for [Tech](techAnalysis.md)\
The analysis for [World](worldAnalysis.md)

busAnalysis

"bus","entertainment","lifestyle","socmed","tech","world"

# Code Used to Create Analyses

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
