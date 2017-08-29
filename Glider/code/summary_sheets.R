#Make Archtype Summary Sheets

## Packages
library(knitr)
library(rmarkdown)

## Data
arch <- read.csv("data/archetype_summary.csv", stringsAsFactors = FALSE)
arch <- arch[order(-arch$ndwel),]

#Trim to top 80%

plot(cumsum(arch$ndwel))
arch <- arch[cumsum(arch$ndwel) < 0.8 * sum(arch$ndwel),]
points(cumsum(arch.trim$ndwel), col = "Red")



## Loop
for (i in 1:nrow(arch)){
  rmarkdown::render(input = "code/summary_sheet_template.Rmd",
                    output_format = "word_document",
                    output_file = paste("archetype-",arch$archcode[i], ".doc", sep=''),
                    output_dir = "handouts/")
}

plot(cumsum(arch$ndwel))
