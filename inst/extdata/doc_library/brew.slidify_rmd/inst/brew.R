## To be used from the main reprotsdirectory

## Load packages
packs <- c("reports", "knitr", "brew", "tools", "slidify", "slidifyLibraries")
invisible(lapply(packs, require, character.only = TRUE))

## Adjust working directory to REPORTS
WD <- getwd(); loc <- file.path(WD, "REPORT")
setwd(loc)
doc <- paste0(basename(WD), ".Rmd")

## function to use brew
makeRmd <- function(group){
  rmd_file = sprintf('%s.Rmd', group)
  brew(doc, rmd_file)
}
 
## loop through all variables and create Rmd deck for each
invisible(lapply(sort(unique(mtcars$cyl)), makeRmd))

## Knit it together
slidify_these <- dir()[(dir() != doc) & file_ext(dir()) == "Rmd"]
invisible(lapply(slidify_these, slidify))

## Reset working directory
setwd(WD)
