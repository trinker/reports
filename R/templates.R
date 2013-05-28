#' View Templates
#' 
#' \code{templates} - View all the reports package templates.
#' 
#' @param print.cons logical.  If \code{TRUE} The template information id 
#' printed to the console.
#' @param names logical.  If \code{TRUE} only the names of the templates will be 
#' returned.
#' @details \code{templates} - prints (via \code{\link[base]{cat}}) to the 
#' console to view possible templates (feel free to submit your own; see 
#' \code{\link[reports]{doc_temp}} for details) as well as optionally returning 
#' a vector of names that the user can pass to other functions.
#' @export
#' @seealso \code{\link[reports]{doc_temp}}
#' @rdname templates
#' @examples
#' \dontrun{
#' templates()
#' templates(FALSE)
#' new_report("new", templates(FALSE)[4])
#' 
#' slidify_templates()
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

#' View Templates
#' 
#' \code{slidify_templates} - View all the built in slidify templates.
#' 
#' @details \code{slidify_templates} - prints a list of minimal and full .Rmd 
#' files included with the reports package for use in presentations created by 
#' the slidify package.  These can be passed to the \code{slidify} argument of 
#' \code{new_reports} and \code{presentation}.
#' @rdname templates
#' @export
slidify_templates <- function() {
    base <- system.file("extdata/slidify_library", package = "reports")
    fls1 <- tools::file_path_sans_ext(sort(dir(file.path(base, c("min")))))
    fls1 <- c(fls1, "default")
    fls2 <- tools::file_path_sans_ext(paste0(".",sort(dir(file.path(base, c("full"))))))
    message("NOTE: see https://github.com/ramnathv/slidifyExamples/tree/gh-pages/examples for slidify examples\n")
    return(list(min=fls1, full = fls2))     
}


