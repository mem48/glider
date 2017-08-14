#Make Archtype Summary Sheets

## Packages
library(knitr)
library(rmarkdown)

## Data
arch <- read.csv("data/archetype_summary.csv", stringsAsFactors = FALSE)
arch <- arch[order(-arch$ndwel),]


## Loop
for (i in 1:nrow(arch)){
  rmarkdown::render(input = "code/summary_sheet_template.Rmd",
                    output_format = "word_document",
                    output_file = paste("handout_", i, ".doc", sep=''),
                    output_dir = "handouts/")
}
