#' View Templates
#' 
#' View all the reports package templates.
#' 
#' @param print.cons logical.  If \code{TRUE} The template information id 
#' printed to the console.
#' @param names logical.  If \code{TRUE} only the names of the templates will be 
#' returned.
#' @details This function prints (via \code{\link[base]{cat}}) to the console to 
#' view possible templates (feel free to submit your own; see 
#' \code{\link[reports]{doc_temp}} for details) as well as optionally returning 
#' a vector of ames that the user can pass to other functions.
#' @export
#' @seealso \code{\link[reports]{doc_temp}}
#' @examples
#' \dontrun{
#' templates()
#' templates(FALSE)
#' new_report("new", templates(FALSE)[4])
#' }
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
