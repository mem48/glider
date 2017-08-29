#Make Archtype Summary Sheets

## Packages
library(knitr)
library(rmarkdown)

## Data
#arch <- read.csv("data/archetype_summary.csv", stringsAsFactors = FALSE)
arch <- readRDS("data/archetype_summary_retrofitopts.Rds")
arch <- arch[order(-arch$ndwel),]

retros <- read.csv("data/retrofit_options.csv", stringsAsFactors = F)

#Trim to top 80%

plot(cumsum(arch$ndwel))
arch <- arch[cumsum(arch$ndwel) < 0.8 * sum(arch$ndwel),]
points(cumsum(arch$ndwel), col = "Red")

m <- 1
n <- 3 #nrow(arch)

## Loop
for (i in m:n){
  rmarkdown::render(input = "code/summary_sheet_template.Rmd",
                    output_format = "word_document",
                    output_file = paste("archetype-",arch$archcode[i], ".doc", sep=''),
                    output_dir = "handouts/")
}

plot(cumsum(arch$ndwel))
