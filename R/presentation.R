#' Presentation Template
#' 
#' Generate a presentation template to increase efficiency.  This is a lighter 
#' weight version of \code{\link[reports]{new_report}}.
#' 
#' @param presentation A character vector of the project name.
#' @param rnw logical.  If \code{TRUE} the docuemnts will be .Rnw and .tex 
#' files.  If \code{FALSE} the documents will be .pptx and .docx files.
#' @param theme \href{http://deic.uab.es/~iblanes/beamer_gallery/index_by_theme.html}{Beamer theme}
#' to use.  If \code{NULL} \code{presentation} will allow the user to choose 
#' interactively.
#' @param bib.loc Optional path to a .bib resource.
#' @param name A character vector of the user's name to be used on the report.
#' @param github.user GitHub user name (character string).
#' @param sources A vector of path(s) to other scripts to be sourced in the 
#' report project upon startup (adds this location to the report project's 
#' \code{.Rprofile}).
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{bib.loc}, \code{github.user}  and \code{sources_reports} in the user's
#' primary \code{.Rprofile}:
#' \enumerate{ 
#'   \item{\bold{bib.loc} - The path to the users primary bibliography}
#'   \item{\bold{github.user} - GitHub user name}
#'   \item{\bold{sources_reports} - Path(s) to additional files/scripts that 
#'   should be included to be sourced in the project startup}
#' }
#' @return Creates a report template.
#' @seealso \code{\link[reports]{new_report}}
#' @export
#' @examples
#' \dontrun{
#' presentation()
#' presentation("New", rnw=TRUE, theme=NULL)
#' presentation("New", rnw=FALSE)
#' }
presentation <- function(presentation = "presentation", rnw = TRUE, 
    theme = "Madrid", bib.loc = getOption("bib.loc"), 
    name = getOption("name_reports"), github.user = getOption("github.user"), 
	sources = getOption("sources_reports"), path = getwd()) {
    if(file.exists(file.path(path, presentation))) {
        cat(paste0("\"", file.path(path, presentation), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("presentation aborted")
        } else {
            delete(paste0(path, "/", presentation))
        }
    }
    if (is.null(theme) & rnw) {
        cat("Choose a theme:\n\n") 
        theme <- c(themes)[menu(c(themes))]      
    }
    x <- suppressWarnings(invisible(folder(folder.name=file.path(path, 
        presentation))))
    OUTLINE <- PRESENTATION <- NULL
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)    
    y <- invisible(folder(OUTLINE, PRESENTATION))
    cat(file = file.path(x, "TO_DO.txt"))
    cat(file = file.path(x, "NOTES.txt"))
    EXF <- "#Source the following project functions on startup"
    cat(EXF, file = file.path(x, "extra_functions.R"))
    root <- system.file("extdata/present", package = "reports")
    pdfloc <- file.path(root, "outline")
    pdfloc2 <- file.path(root, "pres")
    if(rnw){
        invisible(file.copy(file.path(pdfloc, c("outline.tex", 
            "preamble.tex")), y[[1]]))   
        invisible(file.copy(file.path(pdfloc2, c("temp.Rmd",  "temp.Rnw")), 
            y[[2]]))  
        invisible(file.rename(file.path(y[[2]], "temp.Rnw"), 
             file.path(y[[2]], paste0(presentation, ".Rnw"))))          
    } else {
        invisible(file.copy(file.path(pdfloc, c("outline.docx")), y[[1]]))  
        invisible(file.copy(file.path(pdfloc2, c("temp.pptx", "temp.Rmd")), 
            y[[2]])) 
        invisible(file.rename(file.path(y[[2]], "temp.pptx"), 
             file.path(y[[2]], paste0(presentation, ".pptx"))))        
    }
    invisible(file.rename(file.path(y[[2]], "temp.Rmd"), 
         file.path(y[[2]], paste0(presentation, ".Rmd")))) 
    rpro <- c("#Load the packages used",
        "library(reports); library(knitr); library(knitcitations)", 
    	"# library(pander)", "")  
    rpro2 <- c("", "#Source \"extra_functions.R\":",
        "source(file.path(getwd(), \"extra_functions.R\"))")
    rpro3 <- sources
    if (!is.null(rpro3)){
        rpro3 <- c("", "#Source these location(s):", 
            paste0("source(\"", rpro3, "\")"))
    }
    bib <- NULL  
    invisible(file.copy(file.path(root, "TEMP.txt"), x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(presentation, ".Rproj"))))
    if(!is.null(bib.loc) && file.exists(bib.loc)){
        invisible(file.copy(bib.loc, y[[2]]))
        bib <- dir(y[[2]])[tools::file_ext(dir(y[[2]])) == "bib"]
        if (!is.null(getOption("bib.loc"))) {
            bibL <- paste0("options(bib.loc = \"", getOption("bib.loc"), "\")")
            rpro <- c(rpro, bibL)
        }
    }
    if (!is.null(!is.null(github.user) && file.exists(github.user))) {
        git <- paste0("options(github.user = \"", github.user, "\")")
        rpro <- c(rpro, git)
    }       
    cat(paste(c(rpro, rpro2, rpro3), collapse = "\n"), file = file.path(x, 
        ".Rprofile"))
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    if (!is.null(bib)) {
        dr2 <- dir(y[[2]])
        drin2 <- dr2[tools::file_ext(dr2) %in% "Rmd"]
        temp2 <- suppressWarnings(readLines(file.path(y[[2]], drin2)))
        temp2 <- gsub("read.bibtex(.bib)", paste0("read.bibtex(\"", bib, "\")"), 
            temp2, fixed = TRUE)
        cat(paste(temp2, collapse="\n"), file=file.path(y[[2]], drin2))
    }
    if (rnw) {
        drin <- dir(y[[2]])[tools::file_ext(dir(y[[2]])) == "Rnw"]
        temp <- suppressWarnings(readLines(file.path(y[[2]], drin)))
        if (!is.null(bib)) {
            temp <- gsub("\\.bib", bib, temp)
        }
        temp <- gsub("\\\\title\\{.+?\\}", paste0("\\\\title{", gsub("_", " ", 
            presentation),"}") , temp)
        if (!is.null(name)) {
            fxbs <- gsub("\\", "\\\\", paste0("author{", name,"}"), fixed = TRUE)
            temp <- gsub("author\\{.+?\\}", fxbs , temp)
        }
        temp <- gsub("usetheme{}", paste0("usetheme{", theme, "}") , temp, 
            fixed = TRUE)
        cat(paste(temp, collapse="\n"), file=file.path(y[[2]], drin))
    }
    cat(paste0("Presentation \"", presentation, "\" created:\n", x, "\n"))    
}
