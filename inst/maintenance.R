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
