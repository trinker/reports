#==========================
#move project directions
#==========================
outpdf <- paste0(getwd(), "/inst/extdata/docs/")
inpdf <- paste0(getwd(), "/inst/pdf_gen/REPORT_WORKFLOW_GUIDE.pdf")
file.copy(inpdf, outpdf,, TRUE)
file.copy(inpdf, "C:/Users/trinker/Dropbox/Public/packages",, TRUE)

#================================
#download latest reveal template
#================================
download_repo <- function(repo, user){
    require(downloader)
    url <- sprintf("https://github.com/%s/%s/archive/master.zip", user, repo)
    tmp <- tempfile(fileext = ".zip")
    download(url, tmp)
    unzip(tmp) 
    return(tmp) 
}

download_repo("reports", "trinker")

#=========================
#access to internal tools 
#=========================
wheresPandoc <- reports:::wheresPandoc
mgsub <- reports:::mgsub
genX <- reports:::genX
genXtract <- reports:::genXtract
Trim <- reports:::Trim

#==========================
#Check spelling
#==========================
path <- file.path(getwd(), "R")
txt <- suppressWarnings(lapply(file.path(path, dir(path)), readLines))
txt <- lapply(txt, function(x) x[substring(x, 1, 2) == "#'"])
new <- lapply(1:length(txt), function(i){
    c("\n", dir(path)[i], "=========", txt[[i]])
})
out <- paste(unlist(new), collapse="\n")
cat(out, file=file.path(path.expand("C:/Users/trinker/Desktop"), "spelling.doc"))

#==========================
#Get Examples to run
#==========================
library(acc.roxygen2)
examples(path = "C:/Users/trinker/GitHub/reports/R/")

#==========================
#Install Needed Packages
#==========================
library(pacman)
p_load(pander, qdap, installr, ProjectTemplate)

