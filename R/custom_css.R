#' Generate Custom css for RStudio + knitr
#'
#' \code{custom_css} - Generate the components necessary for a custom css for 
#' use with RStudio and knitr.
#'
#' @param rprofile logical.  If \code{TRUE} the \bold{style.R} is added to the 
#' \bold{.Rprofile} for sourcing upon startup.
#' @param loc Path to the report location where the custom css should be placed.  
#' If \code{NULL} only the \bold{style.R} is created in the base directory 
#' \bold{.Rprofile}.
#' @param style.css A path to a \bold{style.css} file that will be used as 
#' the \file{~/css/style.css}.  Also may take a character string indicating one 
#' of the reports package's built in \bold{style.css} (see 
#' \code{\link[reports]{css_styles}}).  Default in 
#' \code{\link[reports]{custom_css}} is a blank \bold{style.css}.
#' @param source logical.  If \code{TRUE} the style.R is sourced intially.
#' @details \code{custom_css} - The user must add the custom contents to the 
#' custom css located in  \file{~/css/style.css}.
#' @note The user has to source before the custom styles will be applied. The 
#' user may specify \code{rfprofile = TRUE} to add the \bold{style.R} to the 
#' \bold{.Rprofile} for sourcing upon startup.  Otherwise, the user must open  
#' the \bold{style.R} as the active tab in RStudio, check the `Source on Save` 
#' box and the click the save icon.  
#' @rdname custom_css
#' @references
#' \url{http://www.rstudio.com/ide/docs/authoring/markdown_custom_rendering} 
#' @export
#' @importFrom markdown markdownToHTML
#' @examples
#' ## custom_css(TRUE, style.css = css_styles()[1])
custom_css <-
function(rprofile = FALSE, loc = QP("REPORT"), style.css = NULL, 
    source = TRUE) {	
	
    if (!is.null(loc)) {	
    	if (!file.exists(loc)) {
            stop(sprintf("%s does not exist", loc))	
    	}
    	## Check css directory existence
        if (!file.exists(file.path(loc, "css"))) {
            cssloc <- file.path(loc, "css")
            dir.create(path = file.path(loc, "css"))
        } else {
        	cssloc <- file.path(loc, "css")
            warning(paste(file.path(loc, "css"), 
                "already exists.\n Directory `css` not generated"))	
        }
        
        ## check style.css existence/create style.css
        if (!file.exists(file.path(loc, "css", "style.css"))) {
        	if (is.null(style.css)) {
                cat("", file=file.path(cssloc, "style.css"))
        	} else {
        		
        		## check is style.css is a reports internal
        		fls <- system.file("extdata/style.css_library", package = "reports")
        		if (any(style.css %in% dir(fls))) {
        		    style.css <- file.path(fls, style.css, "style.css")
        		}
        		sty <- suppressWarnings(readLines(style.css))
                cat(paste(sty, collapse="\n"), file=file.path(cssloc, "style.css"))
        	}
        }  else {
            warning(paste(file.path(loc, "css", "style.css"), 
                "already exists.\n File `~css/style.css` not generated"))	
        }
    }

    ## Create the style.R
    x <- c("options(rstudio.markdownToHTML =", 
        "  function(inputFile, outputFile) {",      
        "    require(markdown)",
        "    markdownToHTML(inputFile, outputFile, stylesheet=file.path(getwd(), \"css/style.css\"))",
        "  }",
        ")\n"
    )
    if (!is.null(loc)) {
        cat(paste(x, collapse = "\n"), file = file.path(loc, "style.R"))
        
        ## Direct where to change css options
            message(paste0("A custom css has been generated for your report.\n\n", 
                "Make changes/additions via:\n", file.path(cssloc, "style.css")))        
    }
    
    ## Add style.R to .Rprofile for sourcing on load
    if (rprofile) {
    	if (sum(dir(all.files = TRUE) == ".Rprofile") > 0) {
        
        ## Test if .Rprofile already contains the style.R function
        RP <- gsub("\\s", "", paste(suppressWarnings(readLines(".Rprofile")), 
            collapse = ""))
        XP <- gsub("\\s", "", paste(x, collapse=""))
        if (!grepl(XP, RP, fixed = TRUE)) {
            cat(paste(c("\n\n", x), collapse = "\n"), file = ".Rprofile", 
                append=TRUE)
        } else {
            warning(paste0(".Rprofile already contains style.R function:\n",
                "`rprofile = TRUE` argument ignored")) 
        }
    	} else {
    		cat(paste(x, collapse = "\n"), file = ".Rprofile")
    	}
    }    
    
    ## Source it for the first time
    if (source) {
        options(rstudio.markdownToHTML =
        function(inputFile, outputFile) {
            markdownToHTML(inputFile, outputFile, stylesheet=file.path(getwd(), "css/style.css"))
          }
        )
    }
}

#' Generate Custom css for RStudio + knitr
#' 
#' \code{css_styles} - View the available built in style.css files.
#' 
##' @details \code{css_styles} - prints a list of available style.css templates 
#' available within the reports package for use with the \code{custom_css} 
#' argument \code{style.css}.  Feel free to submit your own to the reports 
#' package \email{reports.rpackage@@gmail.com} marked with \bold{style.css 
#' template} in the subject line.  
#' 
#' See \url{https://github.com/trinker/style.css_examples} for examples 
#' utilizing the internal reports package style.css.
#' @export
#' @rdname custom_css
#' @examples
#' css_styles()
css_styles <- function() {
    fls <- system.file("extdata/style.css_library", package = "reports")
    dir(fls)
}

#' Generate Custom css for RStudio + knitr
#' 
#' \code{css_style_change} - Change a style.css.
#' 
#' @param cur.style.css The location of the current style.css (this will be 
#' replaced).
#' @export
#' @rdname custom_css
#' @examples
#' ## css_style_change("rinker_vignette")
css_style_change <- 
function(style.css, cur.style.css = QP("REPORT/css/style.css")) {
	
	## Delete current style.css
	delete(cur.style.css)
	
	## check is style.css is a reports internal
    fls <- system.file("extdata/style.css_library", package = "reports")
    if (any(style.css %in% dir(fls))) {
        style.css <- file.path(fls, style.css, "style.css")
    }

	## Copy the new style.css to the old location
	file.copy(style.css, dirname(cur.style.css))
	message(paste0("New style.css copied to:\n", dirname(cur.style.css)))
}

#' Generate Custom css for RStudio + knitr
#' 
#' \code{css_styl_add} - Add a style.css.  Defaults to 
#' \file{~PRESENTATION/assets/css}.
#' 
#' @export
#' @rdname custom_css
css_style_add <- function(loc = QP("PRESENTATION/assets/css")) {
	sty <- file.path(loc, "style.css")
	if (file.exists(sty)) {
	   stop(sprintf("style.css already exists in:\n\n%s", loc))	
	} else {
        cat("/*style.css*/\n", file = sty)
        message(sprintf("style.css created in:\n\n%s", loc))
	}
}


#' Generate Custom css for RStudio + knitr
#'
#' \code{custom_pandoc_style} - Generate the components necessary for a custom 
#' pandoc styling for use with RStudio and knitr.
#'  
#' @rdname custom_css
#' @export
custom_pandoc_style <-
function(rprofile = FALSE, loc = QP("REPORT"), source = TRUE) {	
	
    if (!is.null(loc)) {	
    	if (!file.exists(loc)) {
            stop(sprintf("%s does not exist", loc))	
    	}
    }

    ## Create the pandoc style
    x <- c("options(rstudio.markdownToHTML =", 
        "  function(inputFile, outputFile) { ",     
        "    system(paste(\"pandoc\", shQuote(inputFile), \"-o\", shQuote(outputFile)))",
        "  }",
        ")\n"  
    )
	
    if (!is.null(loc)) {
        cat(paste(x, collapse = "\n"), file = file.path(loc, "style.R"))
        
        ## Direct where to change css options
            message("Custom pandoc styling has been generated for your report.\n\n")        
    }
    
    ## Add style.R to .Rprofile for sourcing on load
    if (rprofile) {
    	if (sum(dir(all.files = TRUE) == ".Rprofile") > 0) {
        
        ## Test if .Rprofile already contains the style.R function
        RP <- gsub("\\s", "", paste(suppressWarnings(readLines(".Rprofile")), 
            collapse = ""))
        XP <- gsub("\\s", "", paste(x, collapse=""))
            if (!grepl(XP, RP, fixed = TRUE)) {
                cat(paste(c("\n\n", x), collapse = "\n"), file = ".Rprofile", 
                append=TRUE)
            } else {
                warning(paste0(".Rprofile already contains style.R function:\n",
                    "`rprofile = TRUE` argument ignored")) 
            }
    	} else {
    		cat(paste(x, collapse = "\n"), file = ".Rprofile")
    	}
    }    
    
    ## Source it for the first time
    if (source) {
        options(rstudio.markdownToHTML = 
          function(inputFile, outputFile) {      
            system(paste("pandoc", shQuote(inputFile), "-o", shQuote(outputFile)))
          }
        )  
    }
}

 

