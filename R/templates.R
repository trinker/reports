#' View Templates
#' 
#' View all the reports package templates.
#' 
#' @import qdap
#' @export
templates <- function(print.cons = TRUE, names = TRUE) {
    root <- system.file("extdata/doc_library", package = "reports")
    if (print.cons) {
    	fls <- paste0(root, "/", dir(root), "/DESCRIPTION")
        z <- invisible(lapply(fls, function(x) {
            y <- suppressWarnings(readLines(x))
            cat(paste0(y, collapse = "\n")); 
            cat("\n========================\n\n")
            return(y)
        }))
    }
    if (names) {
    	z <- dir(root)
    }
    z
}
