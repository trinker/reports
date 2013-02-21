#' View Templates
#' 
#' View all the reports package templates.
#' 
#' @import qdap
#' @export
templates <- function(names = TRUE) {
    root <- system.file("extdata/doc_library", package = "reports")
    z <- invisible(lapply(paste0(root, "/", dir(root), "/DESCRIPTION"), function(x) {
        y <- suppressWarnings(readLines(x))
        cat(paste0(y, collapse = "\n")); 
        cat("\n========================\n\n")
        return(y)
    }))
    if (names) {
    	z <- dir(root)
    }
    z
}
