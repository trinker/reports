#' Generate Custom css for RStudio + knitr
#'
#' Generate the components necessary for a custom css for use with RStudio and 
#' knitr.
#'
#' @param loc Path to the report location where the custom css should be placed.
#' @details The user must add the custom contents to the custom css located in  
#' ~/css/style.css
#' @references
#' \url{http://nsaunders.wordpress.com/2012/08/27/custom-css-for-html-generated-using-rstudio/} 
#' @export
custom_css <- function(loc = file.path(getwd(), "REPORT")) {

    cssloc <- folder(folder.name = file.path(loc, "css"))	
    x <- c("options(rstudio.markdownToHTML =", 
        "  function(inputFile, outputFile) {",      
        "    require(markdown)",
        paste0("    markdownToHTML(inputFile, outputFile, stylesheet=\"", file.path(cssloc, "style.css"), "\")"),
        "  }",
        ")"
    )
    cat("", file=file.path(cssloc, "style.css"))
    cat(paste(x, collapse = "\n"), file = file.path(loc, "style.R"))
    message(paste0("A custom css has been generated for your report.\n\n", 
        "Make changes/additions via:\n", file.path(cssloc, "style.css")))
}