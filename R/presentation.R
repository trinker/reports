#' Presentation Template
#' 
#' Generate a presentation template to increase efficiency.  This is a lighter 
#' weight version of \code{\link[reports]{new_report}} that focuses on the 
#' presentation.
#' 
#' @param presentation A character vector of length two or one: (1) the main directory 
#' name and (2) sub directory names (i.e., all the file contents will be 
#' imprinted with this name). If the length of \code{report} is one this name 
#' will be used as the main directory name and all sub directories and files.
#' @param type A vector of the file format types.  Any combination of the following: 
#' \code{rnw}, \code{rmd} or \code{pptx}.  \code{rnw} corresponds to a beamer slides 
#' (.Rnw file), \code{rmd}  corresponds to a html5 (compliments of slidify) slides 
#' (.Rmd file) and \code{docx} corresponds to PowerPoint slides (.pptx file).
#' @param theme \href{http://deic.uab.es/~iblanes/beamer_gallery/index_by_theme.html}{Beamer theme}
#' to use.  If \code{NULL} \code{presentation} will allow the user to choose 
#' interactively.
#' @param bib.loc Optional path to a \bold{.bib} resource.
#' @param name A character vector of the user's name to be used on the report.
#' @param github.user GitHub user name (character string).
#' @param sources A vector of path(s) to other scripts to be sourced in the 
#' report project upon startup (adds this location to the report project's 
#' \code{.Rprofile}).
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param present  The template to be used in the \bold{PRESENTATION} 
#' .Rmd/.Rpres.  This can be one of the types from \code{slidify_templates} , 
#' \code{"rstudio"}  (this generates a .Rpres file), or a path to an .Rmd/.Rpres 
#' file.  This argument will be overrode if a custom reports template is 
#' supplied with an .Rmd file in the \bold{inst} directory named slidify.Rmd 
#' (\file{~inst/slidify.Rmd}). Or an .Rpres file in the \bold{inst} directory 
#' named rstudio.Rpres (\file{~inst/slidify.Rmd}).  
#' @param open logical.  If \code{TRUE} the project will be opened in RStudio.  
#' The default is to test if \code{presentation} is being used in the global 
#' environment, if it is then the project directory will be opened.  
#' @param github logical.  If \code{TRUE} the repo will be sent to public 
#' \href{https://github.com/}{GitHub} account.
#' @param \ldots Other arguments passed to \code{\link[slidify]{author}}.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{bib.loc}, \code{github.user}, \code{name.reports} 
#' \code{sources.reports},\code{slidify.template} and \code{reveraljs.loc} in 
#' the user's primary \code{.Rprofile}:
#' \enumerate{ 
#'   \item{\bold{bib.loc} - The path to the user's primary bibliography}
#'   \item{\bold{github.user} - GitHub user name}
#'   \item{\bold{name.reports} - The name to use on reports}   
#'   \item{\bold{sources.reports} - Path(s) to additional files/scripts that 
#'   should be included to be sourced in the project startup}
#'   \item{\bold{slidify.template} - Path to, or defualt, .Rmd file tempalte for 
#'   use in as the .Rmd used in the slidify presentations (see 
#'   \code{slidify_templates} for possible non-path arguments)}  
#' }
#' @return Creates a presentation template.
#' @references 
#' \href{https://github.com/ramnathv/slidifyExamples/tree/gh-pages/examples}{slidify examples}\cr
#' \href{http://www.rstudio.com/ide/docs/presentations/overview}{RStudio presentations}
#' @seealso \code{\link[reports]{new_report}},
#' \code{\link[reports]{slidify_templates}},
#' \code{\link[slidify]{author}}
#'
#' \href{https://github.com/hakimel/reveal.js/}{Installation section of reveal.js GitHub}
#' @export
#' @importFrom slidify author
#' @importFrom tools file_ext
#' @examples
#' ## presentation("New")
presentation <- function(presentation = "presentation", type = c("rnw", "rmd"), 
    theme = "Madrid", bib.loc = getOption("bib.loc"), 
    name = getOption("name.reports"), github.user = getOption("github.user"), 
    sources = getOption("sources.reports"), path = getwd(), 
    present = getOption("present.template"), open = is.global(2), github = FALSE,
    ...) {
	
    presentation <- gsub("\\s+", "_", presentation)
    main <- head(presentation, 1)	
    presentation <- tail(presentation, 1)
    slidify <- present 
  
    if(file.exists(file.path(path, main))) {
        message(paste0("\"", file.path(path, presentation), 
            "\" already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("presentation aborted")
        } else {
            delete(file.path(path, presentation))
        }
    }
    if (!is.null(slidify) && slidify %in% dir(path)) {
        slidify <- file.path(path, slidify)   
    }  
    if (is.null(theme) & sum(type %in% "rnw") > 0) {
        message("Choose a theme:\n\n") 
        theme <- c(themes)[menu(c(themes))]      
    }
    suppressWarnings(invisible(dir.create(file.path(path, main),
        recursive = TRUE))) 
    x <- file.path(path, main)
    OUTLINE <- PRESENTATION <- NULL
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)   
    root2 <- system.file("extdata/docs", package = "reports")
    if (sum(type %in% "rmd") < 1) {
        y <- invisible(folder(OUTLINE, PRESENTATION))
    } else {
        if (is.null(slidify)) {
            slidify <- "io2012"
        }
        y <- invisible(folder(OUTLINE))
        y[[2]] <- file.path(x, "PRESENTATION")

        if (!slidify %in% suppressMessages(unlist(slidify_templates(), use.names = FALSE))) {

            if(file.exists(slidify)) {
                slid.path <- slidify
            } else {
                slid <- system.file("extdata/rstudio_pres_library", package = "reports")
                slid.path <- file.path(slid, slidify)
                slid.path <- file.path(slid.path, dir(slid.path)[file_ext(dir(slid.path)) %in% "Rpres"])
            }
            invisible(folder(folder.name=y[[2]]))
            suppressMessages(local_host(y[[2]]))
            setwd(x)
            Rpres <- suppressWarnings(readLines(slid.path)) 
            title. <- grepl("title", Rpres) & !grepl("subtitle", Rpres)
            Rpres[title.] <- gsub("title", presentation, Rpres[title.])
            if(!is.null(name)) {
                name. <- grepl("author", Rpres) & grepl("\\:", Rpres)
                Rpres[name.] <- paste0("author: ", 
                    strsplit(name, "\\\\")[[1]][1], "    ")
            }
            cat(paste(Rpres, collapse="\n"), file = file.path(y[[2]], paste0(presentation, ".Rpres")))

        } else {

            suppressMessages(author(y[[2]], use_git = FALSE, open_rmd = FALSE, ...))
            suppressMessages(slidify_layouts(file.path(y[[2]], "assets/layouts")))
            if(file.exists(slidify)) {
                slid.path <- slidify
            } else {
                if (slidify == "default") {
                    slid.path <- file.path(y[[2]], "index.Rmd")
                } else {
                    slid <- system.file("extdata/slidify_library", package = "reports")
                    if (substring(slidify, 1, 1) == ".") {
                        back <- "full"
                        slidify <- substring(slidify, 2)
                    } else {
                        back <- "min"
                    }
                    slid.path <- file.path(slid, back, paste0(slidify, ".Rmd"))
                }
            }
            suppressMessages(local_host(y[[2]]))
            setwd(x)
            Rmd <- suppressWarnings(readLines(slid.path)) 
            Rmd <- c(Rmd, "\n")
            title. <- grepl("title", Rmd) & grepl("\\:", Rmd) & !grepl("subtitle", Rmd)
            specials <- c("brew")
            if (!slidify %in% specials) {
                Rmd[title.] <- paste0("title      : ", presentation)
            } else {
                titlepieces <- unlist(strsplit(Rmd[title.], ":"))
                Rmd[title.] <- paste0("title      : ", presentation, titlepieces[2])
                slidextras <- system.file("extdata/r_script_library/slidify", package = "reports")
                sliddest <- file.path(slidextras, slidify)
                suppressWarnings(file.copy(file.path(sliddest, dir(sliddest)), y[[2]], recursive = TRUE))
            }
            if(!is.null(name)) {
                name. <- grepl("author", Rmd) & grepl("\\:", Rmd)
                Rmd[name.] <- paste0("author     : ", strsplit(name, "\\\\")[[1]][1])
            }
            if(slidify == "revealjs") {
                title2. <- Rmd %in% "# title"
                Rmd[title2.] <- paste0("# ", presentation)
                if(!is.null(name)) {
                    name2. <- Rmd %in% "### name"
                    Rmd[name2.] <- paste0("### ", strsplit(name, "\\\\")[[1]][1])
                }
                css <- system.file("extdata/docs/style.css", package = "reports")
                file.copy(css,  file.path(y[[2]], "assets/css"))            
            }
            cat(paste(Rmd, collapse="\n"), file = file.path(y[[2]], paste0(presentation, ".Rmd")))
            delete(file.path(y[[2]], "index.Rmd"))
        }

        ## Location of core PRESENTATION documents documents
        coreroot <- system.file("extdata/core_PRESENTATION", package = "reports")
        css <- file.path(coreroot, "css")
        js <- file.path(coreroot, "js")

        ## Copy All Core PRESENTATION Contents
        file.copy(file.path(css, dir(css)), file.path(y[[2]], "assets/css")) 
        file.copy(file.path(js, dir(js)), file.path(y[[2]], "assets/js"))  
    }

    cat(file = file.path(x, "TO_DO"))
    cat(file = file.path(x, "NOTES"))
    EXF <- "#Source the following project functions on startup"
    cat(EXF, file = file.path(x, "extra_functions.R"))
    root <- system.file("extdata/present", package = "reports")
    pdfloc <- file.path(root, "outline")
    pdfloc2 <- file.path(root, "pres")
    pdfloc5 <- file.path(root2, "PRESENTATION_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc5, x))
    if(sum(type %in% c("rnw")) > 0){
        invisible(file.copy(file.path(pdfloc, c("outline.tex", 
            "preamble.tex")), y[[1]]))   
        invisible(file.copy(file.path(pdfloc2, c("temp.Rnw")), 
            y[[2]]))  
        invisible(file.rename(file.path(y[[2]], "temp.Rnw"), 
             file.path(y[[2]], paste0(presentation, ".Rnw"))))          
    } 
    if(sum(type %in% "pptx") > 0){
        invisible(file.copy(file.path(pdfloc, c("outline.docx")), y[[1]]))  
        invisible(file.copy(file.path(pdfloc2, c("temp.pptx")), 
            y[[2]])) 
        invisible(file.rename(file.path(y[[2]], "temp.pptx"), 
             file.path(y[[2]], paste0(presentation, ".pptx"))))        
    }
    dir.create(file.path(y[[2]], "figure"), FALSE)
    rpro <- c("#Load the packages used",
        "library(reports); library(slidify); library(slidifyLibraries); library(knitr); library(knitcitations)", 
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
        bib <- dir(y[[2]])[file_ext(dir(y[[2]])) == "bib"]
        if (!is.null(getOption("bib.loc"))) {
            bibL <- paste0("options(bib.loc = \"", getOption("bib.loc"), "\")")
            rpro <- c(rpro, bibL)
        }
    }
    if (!is.null(!is.null(github.user) && file.exists(github.user))) {
        git <- paste0("options(github.user = \"", github.user, "\")")
        rpro <- c(rpro, git)
    }       
    lh <- c("", "suppressMessages(local_host())", "")
    cat(paste(c(rpro, rpro2, lh, rpro3), collapse = "\n"), file = file.path(x, 
        ".Rprofile"))
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    if (!is.null(bib) && sum(type %in% "rmd") > 0) {
        dr2 <- dir(y[[2]])
        drin2 <- dr2[file_ext(dr2) %in% c("Rmd", "Rpres")][1]
        temp2 <- suppressWarnings(readLines(file.path(y[[2]], drin2)))
        temp2 <- gsub("read.bibtex(.bib)", paste0("read.bibtex(\"", bib, "\")"), 
            temp2, fixed = TRUE)
        cat(paste(temp2, collapse="\n"), file=file.path(y[[2]], drin2))
    }
    if (sum(type %in% "rnw") > 0) {
        drin <- dir(y[[2]])[file_ext(dir(y[[2]])) == "Rnw"]
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
    o <- paste0("Presentation \"", presentation, "\" created:\n", x, "\n")
    class(o) <- "reports"
        
    ## Send to github
    if (github) {
    	try(repo2github(project.dir = x))
    }
    
    ## Open Project in RStudio
    if (open) {
        open_project(file.path(x, paste0(presentation, ".Rproj")))
    }  
    
    return(o)    
}
