#' Vignette Template
#' 
#' \code{new_vignette} - Generate a vignettes template to increase efficiency.
#' 
#' @param vignette A character string of the vignette name (subdirectories of 
#' the main directory, "vignettes", will utilize this string).  The function 
#' \code{append_vignette} will create an additional vignette with this name in 
#' the vignettes directory.
#' @param type A character string of either \code{"rmd"} or \code{"rnw"}.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.  Suggest setting to the package root directory. If
#' using the function \code{append_vignette} this is the path to the current 
#' vignettes directory. 
#' @param bib.loc Optional path to a .bib resource.
#' @param name A character string of the user's name to be used on the vignette.
#' @param open logical.  If \code{TRUE} the project will be opened in RStudio.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{bib.loc}, \code{name.reports} in the user's primary \code{.Rprofile}:
#' \enumerate{ 
#'   \item{\bold{bib.loc} - The path to the users primary bibliography}
#'   \item{\bold{name.reports} - The name to use on reports}
#' }
#' @return Creates a vignette template.
#' @rdname new_vignette
#' @references 
#' \url{http://yihui.name/knitr/demo/vignette/} 
#' 
#' \url{http://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Writing-package-vignettes}
#' @seealso \code{\link[utils]{browseVignettes}}
#' @export
#' @importFrom tools file_ext
#' @examples 
#' ## new_vignette()
new_vignette <-
function(vignette = "vignette", type = "rmd", path = getwd(),
    bib.loc = NULL, name = getOption("name.reports"), open = FALSE) {

    ## preparing type
    type <- tolower(type)
    TYPE <- type

    ## Check type make sure valid rmd or rnw
    if (!type %in% c("rnw", "rmd")) {
        stop("Please select either \"rnw\" or \"rmd\" for type")
    }

    ## reconfigure vignette name and type 
    ## specify main dir name as vignettes
    type <- paste0("vignette_", type)
    vignette <- gsub("\\s+", "_", vignette)
    main <- "vignettes"   
  
    ## Check if the directory vignettes already exists in that location
    if(file.exists(file.path(path, main))) {
        message(paste0("\"", file.path(path, main), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_vignette aborted")
        } else {
            delete(file.path(path, main))
        }
    }
  
    ## Create root `vignettes` directory
    suppressWarnings(invisible(dir.create(file.path(path, main),
        recursive = TRUE))) 

    ## set up working directory
    x <- file.path(path, main)
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)

    ## find the vignettes library in the reports package
    root <- system.file("extdata/vign_library", package = "reports")

    ## grab the appropriae vignette library & read in DESCRIPTION file
    pdfloc <- file.path(root, type)
    desc <- suppressWarnings(readLines(file.path(pdfloc, "DESCRIPTION"))) 

    ## add ToDo and Notes
    cat(file = file.path(x, "TO_DO"))
    cat(file = file.path(x, "NOTES"))

    ## Add the extra functions
    EXF <- "#Source the following project functions on startup"
    cat(EXF, file = file.path(x, "extra_functions.R"))

    ## create the main vignette docuemnt and add author name/vignette name
    invisible(file.copy(file.path(pdfloc, dir(pdfloc)), x, TRUE, TRUE))   
    dir.create(file.path(x, "figure"), FALSE)
    old.name <- dir(x)[dir(x) %in% c("doc.Rmd", "doc.Rnw")]
    new.name <- gsub("doc\\.", paste0(vignette, "."), old.name)
    file.rename(file.path(x, old.name), file.path(x, new.name))
    doc <- suppressWarnings(readLines(file.path(x, new.name)))
    if (TYPE == "rmd") {
        if (!is.null(name)) {
            nameL <- grepl("## name", doc)
            doc[nameL] <- paste("#", strsplit(name, "\\\\")[[1]][1])
        }
        vign_name <- grepl("# vignette_name", doc)
        doc[vign_name] <- paste("#", vignette, "Package Vignette")
    } else {
        if (!is.null(name)) {
            nameL <- grepl("\\author{}" , doc, fixed=TRUE)
            doc[nameL] <- paste0("\\author{", name, "}")
        }
        vign_name <- grepl("\\title{vignette name}", doc, fixed=TRUE)
        doc[vign_name] <- paste0("\\title{", vignette, "}")
    }
    cat(paste(doc, collapse="\n"), file = file.path(x, new.name))

    ## create the .Rproj
    root2 <- system.file("extdata/docs", package = "reports")
    pdfloc2 <- file.path(root2, "TEMP.txt")
    invisible(file.copy(pdfloc2, x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(vignette, ".Rproj"))))

    ## create the .Rprofile and add bib
    rpro <- c("#Load the packages used",
        "library(reports); library(knitr); library(knitcitations)", "")  
    rpro2 <- c("", "#Source \"extra_functions.R\":",
        "source(file.path(getwd(), \"extra_functions.R\"))")
    bib <- NULL
    if(!is.null(bib.loc) && file.exists(bib.loc)){
        invisible(file.copy(bib.loc, x))
        bib <- dir(x)[file_ext(dir(x)) == "bib"]
        if (!is.null(bib.loc)) {
            bibL <- paste0("options(bib.loc = \"", bib.loc, "\")")
            rpro <- c(rpro, bibL)
        }
    }
    cat(paste(c(rpro, rpro2), collapse = "\n"), 
        file = file.path(x, ".Rprofile"))
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }

    o <- paste0("Directory \"", "vignettes", "\" created:\n", x, "\n")
    class(o) <- "reports"
    return(o)    
}

#' Vignette Template
#'
#' \code{append_vignette} - Add vignettes to a pre-existing vignettes directory.
#' 
#' @export
#' @rdname new_vignette
append_vignette <-
function(vignette = "vignette", type = "rmd", path = getwd(),
    bib.loc = NULL, name = getOption("name.reports")) {

	x <- NULL
	
    ## Check if path exists, is a directory & is named vignettes
    if (!file.exists(path)) {
        stop("path does not exist")
    }
    if(!file_test("-d", c(path))) {
        stop("path is not a directory")
    }
    if (basename(path) != "vignettes") {
        cat(paste(path, 
            "does not appear to be named \"vignettes\":\n\nDo you wish to continue?\n"))
            ans <- menu(c("Yes", "No")) 
            if (ans == "2") {
                stop("new_vignette aborted")
            } 
    }

    ## preparing type
    type <- tolower(type)
    TYPE <- type

    ## Check type make sure valid rmd or rnw
    if (!type %in% c("rnw", "rmd")) {
        stop("Please select either \"rnw\" or \"rmd\" for type")
    }

    ## reconfigure vignette name and type 
    ## specify main dir name as vignettes
    type <- paste0("vignette_", type)
    vignette <- gsub("\\s+", "_", vignette)
   
    ## Check if the vignettes already exists in that location
    if(file.exists(file.path(path, vignette))) {
        cat(paste0("\"", file.path(path, vignette), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_vignette aborted")
        } else {
            delete(file.path(path, vignette))
        }
    }
  
    ## set up working directory
    WD <- getwd(); on.exit(setwd(WD))
    setwd(path)

    ## find the vignettes library in the reports package
    root <- system.file("extdata/vign_library", package = "reports")

    ## grab the appropriae vignette library & read in DESCRIPTION file
    pdfloc <- file.path(root, type)
    desc <- suppressWarnings(readLines(file.path(pdfloc, "DESCRIPTION"))) 

    ## create the main vignette document 
    doc <- dir(pdfloc)[grepl("doc\\.", dir(pdfloc))]
    invisible(file.copy(file.path(pdfloc, doc), path, TRUE, TRUE)) 

    ## rename it add author name/vignette name
    old.name <- dir(path)[dir(path) %in% c("doc.Rmd", "doc.Rnw")]
    new.name <- gsub("doc\\.", paste0(vignette, "."), old.name)
    file.rename(file.path(path, old.name), file.path(path, new.name))
    doc <- suppressWarnings(readLines(file.path(path, new.name)))
    if (TYPE == "rmd") {
        if (!is.null(name)) {
            nameL <- grepl("## name", doc)
            doc[nameL] <- paste("#", strsplit(name, "\\\\")[[1]][1])
        }
        vign_name <- grepl("# vignette_name", doc)
        doc[vign_name] <- paste("#", vignette, "Package Vignette")
    } else {
        if (!is.null(name)) {
            nameL <- grepl("\\author{}" , doc, fixed=TRUE)
            doc[nameL] <- paste0("\\author{", name, "}")
        }
        vign_name <- grepl("\\title{vignette name}", doc, fixed=TRUE)
        doc[vign_name] <- paste0("\\title{", vignette, "}")
    }
    cat(paste(doc, collapse="\n"), file = file.path(path, new.name))

    ## Add DESCRIPTION if needed

    dcoty <- "Doc Type:"
    poss <- c("rmd", "rnw")
    if ("DESCRIPTION" %in% dir(path)) {  
        desc2 <- suppressWarnings(readLines(file.path(path, "DESCRIPTION"))) 
        acts <- poss[poss %in% Trim(gsub(dcoty,  "", desc2[grepl(dcoty, desc2)]))]
        if(!TYPE %in% acts) {
            mess <- "This Vignettes directory contains both an Rnw and Rmd vignette:\n\n"
            cat(paste(c(mess, desc2, "", desc), collapse="\n"), 
                file = file.path(path, "DESCRIPTION"))
        }
    } else {
        cat(paste(desc, collapse="\n"), file = file.path(path, "DESCRIPTION"))
    }
 
    ## add css if necessary
    if ("css" %in% dir(path)) {
        csspath <- file.path(path, "css")
    }
    if ((TYPE == "rmd" && !"css" %in% dir(path)) | !file_test("-d", c(csspath))) {
        invisible(file.copy(file.path(pdfloc, "css"), path, TRUE, TRUE)) 
    }

    ## bibliography handling
    bib <- NULL
    if(!is.null(bib.loc) && file.exists(bib.loc)){
        invisible(file.copy(bib.loc, path))
    }
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }

    o <- paste0("vignette append \"", vignette, "\" added to:\n", path, "\n")
    class(o) <- "reports"
    if (open) {
        open_project(file.path(x, vignette, paste0(vignette, ".Rproj")))
    }      
    return(o)    
}

