#' Report Template
#' 
#' Generate a report/paper template to increase efficiency.
#' 
#' @param report A character vector of the project name.
#' @param bib.loc Optional path to a .bib resource.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param name A character vector of the user's name to be used on the report.
#' @note The user may want to set \code{\link[base]{options}} for \code{bib.loc} 
#' and \code{name_reports} in the .Rprofile.
#' @return Creates a report template.
#' @import qdap
#' @export
new_report <- function(report = "report", bib.loc = getOption("bib.loc"),  
    name = getOption("name_reports"), path = getwd()) {
    WD <- getwd()
    on.exit(setwd(WD))
    if(file.exists(paste0(path, "/", report))) {
        cat(paste0("\"", paste0(path, "/", report), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_report aborted")
        } else {
            delete(paste0(path, "/", report))
        }
    }
    x <- suppressWarnings(invisible(folder(folder.name=paste0(path, "/", report))))
    setwd(x) 
    REPORT <- ARTICLES <- OUTLINE <- PRESENTATION <- NULL
    y <- invisible(folder(REPORT, ARTICLES, OUTLINE, PRESENTATION))
    cat("http://", file = paste0(y[[2]], "/websites.txt"))
    cat(file = paste0(x, "/TO_DO.txt"))
    cat(file = paste0(x, "/NOTES.txt"))
    root <- system.file("extdata/docs", package = "reports")
    pdfloc <- paste0(root, "/", c("preamble.tex", "template.tex"))
    invisible(file.copy(pdfloc, y[[1]]))   
    pdfloc2 <- paste0(root, "/", c("preamble2.tex", "outline.tex"))
    invisible(file.copy(pdfloc2, y[[3]]))  
    pdfloc3 <- paste0(root, "/", c("temp.Rmd", "temp.Rnw"))
    invisible(file.copy(pdfloc3, y[[4]])) 
    invisible(file.rename(paste0(y[[4]], "/temp.Rmd"), 
        paste0(y[[4]], "/",  report, ".Rmd")))  
    invisible(file.rename(paste0(y[[4]], "/temp.Rnw"), 
        paste0(y[[4]], "/",  report, ".Rnw")))      
    pdfloc4 <- paste0(root, "/TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    pdfloc5 <- paste0(root, "/PROJECT_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc5, x))
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  report, ".Rproj"))) 
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  report, ".Rproj")))
    rpro <- c("#load the packages used",
        "library(reports)",
        "library(qdap)")
    bib <- NULL
    if(!is.null(bib.loc) & file.exists(bib.loc)){
        invisible(file.copy(bib.loc, y[[1]]))
        bib <- dir(y[[1]])[tools::file_ext(dir(y[[1]])) == "bib"]
        if (!is.null(getOption("bib.loc"))) {
            bibL <- paste0("options(bib.loc = \"", getOption("bib.loc"), "\")")
            rpro <- c(rpro, bibL)
        }
    }
    cat(paste(rpro, collapse = "\n"), file = paste0(x, "/.Rprofile"))
    if (!is.null(bib.loc) & !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    temp <- readLines(paste0(y[[1]], "/template.tex"))
    if (!is.null(bib)) {
        temp <- gsub("\\.bib", bib, temp)
    }
    temp <- gsub("title{T}", paste0("title{", report, "}"), temp, fixed = TRUE)
    if (!is.null(name)) {
        temp <- gsub("author{NAME", paste0("author{", name), temp, fixed = TRUE)
    }
    cat(paste(temp, collapse="\n"), file=paste0(y[[1]], "/template.tex"))
    invisible(file.rename(paste0(y[[1]], "/template.tex"), 
        paste0(y[[1]], "/",  report, ".tex")))
    cat(paste0("Report \"", report, "\" created:\n", x, "\n"))    
}