#' Report Template
#' 
#' Generate a report/paper template to increase efficiency.
#' 
#' @param report A character vector of the project name.
#' @param template A character vector of the internal reports template or an 
#' external path to a template in the reports package style.
#' @param bib.loc Optional path to a .bib resource.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param name A character string of the user's name to be used on the report.
#' @param sources A vector of path(s) to other scripts to be sourced in the 
#' report project upon startup (adds this location to the report project's 
#' \code{.Rprofile}).
#' @param AN.xlsx logical.  If \code{TRUE} the article notes (AN) will be in 
#' .xlsx format.  If \code{FALSE} the document will be a .csv file..
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{bib.loc}, \code{name_reports} and \code{sources_reports} in the users
#' primary \code{.Rprofile}.
#' @return Creates a report template.
#' @seealso \code{\link[reports]{doc_temp}}
#' @section Additional Guide: Introductory video
#' \url{http://youtu.be/cokNUTGtoM4}
#' @import qdap
#' @export
#' @examples 
#' \dontrun{
#' new_report()
#' }
new_report <- function(report = "report", template = "apa6.mod.qual_tex", 
    bib.loc = getOption("bib.loc"), name = getOption("name_reports"), 
    sources = getOption("sources_reports"), path = getwd(), AN.xlsx = TRUE) {
    report <- gsub("\\s+", "_", report)
    if(file.exists(file.path(path, report))) {
        cat(paste0("\"", file.path(path, report), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_report aborted")
        } else {
            delete(paste0(path, "/", report))
        }
    }
    x <- suppressWarnings(invisible(folder(folder.name=file.path(path, report))))
    REPORT <- ARTICLES <- OUTLINE <- PRESENTATION <- NULL
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)
    y <- invisible(folder(REPORT, ARTICLES, OUTLINE, PRESENTATION))
    AN <- system.file("extdata/docs", package = "reports")
    if (AN.xlsx) {
        invisible(file.copy(file.path(AN, "notes.xlsx"), y[[2]])) 
    } else {
        invisible(file.copy(file.path(AN, "notes.csv"), y[[2]])) 
    }
    cat("http://", file = file.path(y[[2]], "websites.txt"))
    cat(file = file.path(x, "TO_DO.txt"))
    cat(file = file.path(x, "NOTES.txt"))
    EXF <- "#Source the following project functions on startup"
    cat(EXF, file = file.path(x, "extra_functions.R"))
    dat <- data.frame(inf=c("doc", "rnw", "tex"), outf=c(TRUE, FALSE, FALSE), 
        stringsAsFactors = FALSE)
    root <- system.file("extdata/doc_library", package = "reports")
    if (!template %in% dir(root)) {
        if (!file.exists(root)) {
            stop("template is not a valid reports package template or path")
        } else {
            root <- template
        }        
    }
    root2 <- system.file("extdata/docs", package = "reports")
    t2 <- type <- tail(unlist(strsplit(basename(template), "_")), 1)
    if (all(!dat[, 1] %in% type)) {
        stop("template must end in \"_doc\", \"_tex\" or \"_rnw\"")
    }
    type <- dat[dat[, 1] %in% type, 2]
    pdfloc <- file.path(root, template)
    invisible(file.copy(pdfloc, y[[1]]))   
    if (type) {
       fls <- "outline.docx"
    } else {
       fls <- c("preamble.tex", "outline.tex")
    } 
    invisible(file.copy(file.path(root2, fls) , y[[3]]))
    pdfloc3 <- file.path(root2, c("temp.Rmd", "temp.Rnw", "temp.pptx"))
    invisible(file.copy(pdfloc3[1], y[[4]])) 
    invisible(file.rename(file.path(y[[4]], "temp.Rmd"), 
        file.path(y[[4]], paste0(report, ".Rmd")))) 
    if (type) {
        invisible(file.copy(pdfloc3[3], y[[4]]))
        invisible(file.rename(file.path(y[[4]], "temp.pptx"), 
            file.path(y[[4]], paste0(report, ".pptx"))))        
    } else {
        invisible(file.copy(pdfloc3[2], y[[4]]))
        invisible(file.rename(file.path(y[[4]], "temp.Rnw"), 
            file.path(y[[4]], paste0(report, ".Rnw"))))
    }
    pdfloc4 <- file.path(root2, "TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(report, ".Rproj"))))
    pdfloc5 <- file.path(root2, "PROJECT_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc5, x))
    rpro <- c("#Load the packages used",
        "library(reports); library(qdap); library(knitr); library(knitcitations)", 
        "")  
    rpro2 <- c("", "#Source \"extra_functions.R\":",
        "source(file.path(getwd(), \"extra_functions.R\"))")
    rpro3 <- sources
    if (!is.null(rpro3)){
        rpro3 <- c("", "#Source these location(s):", 
            paste0("source(\"", rpro3, "\")"))
    }
    bib <- NULL
    if(!is.null(bib.loc) && file.exists(bib.loc)){
        invisible(file.copy(bib.loc, y[[1]]))
        invisible(file.copy(bib.loc, y[[4]]))
        bib <- dir(y[[1]])[tools::file_ext(dir(y[[1]])) == "bib"]
        if (!is.null(getOption("bib.loc"))) {
            bibL <- paste0("options(bib.loc = \"", getOption("bib.loc"), "\")")
            rpro <- c(rpro, bibL)
        }
    }
    cat(paste(c(rpro, rpro2, rpro3), collapse = "\n"), 
        file = file.path(x, ".Rprofile"))
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    if (!is.null(bib)) {
        dr2 <- dir(y[[4]])
        drin2 <- dr2[tools::file_ext(dr2) %in% "Rmd"]
        temp2 <- suppressWarnings(readLines(file.path(y[[4]], drin2)))
        temp2 <- gsub("read.bibtex(.bib)", paste0("read.bibtex(\"", bib, "\")"), 
        temp2, fixed = TRUE)
        cat(paste(temp2, collapse="\n"), file=file.path(y[[4]], drin2))
    } 
    if (!type) {
        fp <- file.path(root, template)
        invisible(file.copy(file.path(fp, dir(fp)), paste0(y[[1]], "/")))
        dr <- dir(y[[1]])
        drin <- dr[substring(dr, 1, nchar(dr) -4) %in% "doc"]
        temp <- suppressWarnings(readLines(file.path(y[[1]], drin)))
        if (!is.null(bib)) {
            temp <- gsub("\\.bib", bib, temp)
        }
        temp <- gsub("title\\{.+?\\}", paste0("title{", gsub("_", " ", report),
            "}") , temp)
        if (!is.null(name)) {
            fxbs <- gsub("\\", "\\\\", paste0("author{", name,"}"), fixed = TRUE)
            temp <- gsub("author\\{.+?\\}", fxbs , temp)
        }
        cat(paste(temp, collapse="\n"), file=file.path(y[[1]], drin))
        invisible(file.rename(file.path(y[[1]], drin), 
            file.path(y[[1]], paste0(report, ".", 
            tools::file_ext(drin)))))       
        dr2 <- dir(y[[4]])
        drin3 <- dr2[tools::file_ext(dr2) %in% "Rnw"]
        temp3 <- suppressWarnings(readLines(file.path(y[[4]], drin3)))
        if (!is.null(name)) {
            temp3 <- gsub("author{Author}", paste0("author{", name, "}"), 
                temp3, fixed = TRUE)
        }
        if (!is.null(bib)) {
            temp3 <- gsub("addbibresource{.bib}", paste0("addbibresource{", bib, 
                "}"), temp3, fixed = TRUE) 
        }
        cat(paste(temp3, collapse="\n"), file=file.path(y[[4]], drin3))
    } else {
        fp <- file.path(root, template)
        invisible(file.copy(file.path(fp, dir(fp)), paste0(y[[1]], "/")))        
    }
    ins <- file.path(pdfloc, "inst")
    if (file.exists(ins)) {
        ins2 <- file.path(ins, dir(ins))
        invisible(lapply(ins2, function(zz) {
           file.copy(zz, x, overwrite = TRUE, recursive = TRUE) 
        }))
    }
    cat(paste0("Report \"", report, "\" created:\n", x, "\n"))    
}