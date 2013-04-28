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
p_load(pander, qdap, installr, ProjectTemplate, slidify)

#========================
#staticdocs dev version
#========================
#packages
library(highlight); library(staticdocs)

#STEP 1: create static doc  
build_package(package="C:/Users/trinker/GitHub/reports", 
    base_path="C:/Users/trinker/Desktop/reports_dev/", examples = TRUE)

library(reports); library(qdap); library(acc.roxygen2)
#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/reports_dev"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/reports/inst/extra_statdoc/readme.R"
expand_statdoc(path2, readme = rdme, 
    to.icon = c("sync_img", "sync_rnp", "sync_all", "VS", "VM", "IM2"))

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "reports_dev"))
file.copy(path, file, TRUE, TRUE)
delete(path)

#STEP 4: copy dependencies page to trinker.guthub
file2 <- "C:/Users/trinker/GitHub/trinker.github.com/reports_dev"
path2 <- "C:/Users/trinker/GitHub/reports/inst/dependencies/dependencies.html"
file.copy(path2, file2, TRUE, TRUE)
#==========================
#staticdocs current version
#==========================
#packages
library(highlight); library(staticdocs)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
#NOTE:  If this occurrs:
## Loading reports
## Error in importIntoEnv(pkgenv, exports, nsenv, exports) : 
##   cannot change value of locked binding for 'CA'
#
# RESTART AND DON"T LOAD QDAP
build_package(package="C:/Users/trinker/GitHub/reports", 
    base_path="C:/Users/trinker/Desktop/reports/", examples = TRUE)

library(reports); library(qdap); library(acc.roxygen2)
#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/reports"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/reports/inst/extra_statdoc/readme.R"
#extras <- qcv(folder, QQ)
expand_statdoc(path2, readme = rdme, 
    to.icon = c("sync_img", "sync_rnp", "sync_all", "VS", "VM", "IM2"))

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "reports"))
file.copy(path, file, TRUE, TRUE)
delete(path)

#STEP 4: copy dependencies page to trinker.guthub
file2 <- "C:/Users/trinker/GitHub/trinker.github.com/reports"
path2 <- "C:/Users/trinker/GitHub/reports/inst/dependencies/dependencies.html"
file.copy(path2, file2, TRUE, TRUE)
#==========================
#==========================
#move project directions
#==========================
outpdf <- paste0(getwd(), "/inst/extdata/docs/")
inpdf <- paste0(getwd(), "/inst/pdf_gen/PROJECT_WORKFLOW_GUIDE.pdf")
file.copy(inpdf, outpdf,, TRUE)
file.copy(inpdf, "C:/Users/trinker/Dropbox/Public/packages",, TRUE)
