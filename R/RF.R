#' Slidify Shortcut to Include reveal.js Animated Fragments
#' 
#' Slidify uses \code{<script> $('ul.incremental li').addClass('fragment') </script>}
#' to include animated fragments (see \href{https://github.com/ramnathv/slidifyExamples/blob/gh-pages/examples/revealjs/index.Rmd}{this example})
#' in reveal.js slides.  This is required per each slide.  Using \code{`r RF()`} 
#' reduces the typing invloved with this action.
#'
#' @param print logical.  If \code{TRUE} \code{\link[base]{cat}} prints the output to the 
#' console.  If \code{FALSE} returns to the console. 
#' @param copy2clip logical.  If \code{TRUE} attempts to copy the output to the 
#' clipboard. 
#' @export
#' @examples
#' ## RF()
RF <- function(print = FALSE, copy2clip = interactive()) {
    x <- c("<script>", "$('ul.incremental li').addClass('fragment')",
        "</script>")
    x <- paste0(paste(x, collapse = "\n"), "\n")
    if(copy2clip){
        write_clip(x)
    }
    prin(x, print = print)
}
