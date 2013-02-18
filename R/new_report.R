#' Report Template
#' 
#' Generate a report/paper template to increase efficiency.
#' 
#' @param name A character vector of the project name.
#' @param bibloc Optional path to a .bib resource.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @return Creates a report template.
#' @import qdap
#' @export
new_report <- function(name = "report", bib.loc = getOption("bib.loc"),  path = getwd()) {
    WD <- getwd()
    on.exit(setwd(WD))
    if(file.exists(paste0(path, "/", name))) {
        cat(paste0("\"", paste0(path, "/", name), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_report aborted")
        } else {
            delete(paste0(path, "/", name))
        }
    }
    x <- suppressWarnings(invisible(folder(folder.name=paste0(path, "/", name))))
    setwd(x) 
    REPORT <- ARTICLES <- NULL
    y <- invisible(folder(REPORT, ARTICLES))
    root <- system.file("extdata/docs", package = "reports")
    pdfloc <- paste0(root, "/", c("preamble.tex", "template.tex"))
    invisible(file.copy(pdfloc, y[[1]]))   
    pdfloc4 <- paste0(root, "/TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  name, ".Rproj"))) 
    invisible(file.rename(paste0(x, "/TEMP.txt"), 
        paste0(x, "/",  name, ".Rproj")))
    rpro <- c("#load the packages used",
        "library(reports)",
        "library(qdap)")
    cat(paste(rpro, collapse = "\n"), file = paste0(x, "/.Rprofile"))
    bib <- NULL
    if(!is.null(bib.loc) & file.exists(bib.loc)){
        invisible(file.copy(bib.loc, y[[1]]))
        bib <- dir(y[[1]])[tools::file_ext(dir(y[[1]])) == "bib"]
    }
    if (!is.null(bib.loc) & !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    temp <- readLines(paste0(y[[1]], "/template.tex"))
    if (!is.null(bib)) {
        temp <- gsub("\\.bib", bib, temp)
    }
    temp <- gsub("title{T}", paste0("title{", name, "}"), temp, fixed = TRUE)
    cat(paste(temp, collapse="\n"), file=paste0(y[[1]], "/template.tex"))
    invisible(file.rename(paste0(y[[1]], "/template.tex"), 
        paste0(y[[1]], "/",  name, ".tex")))
    cat(paste0("Report \"", name, "\" created:\n", x, "\n"))    
}
