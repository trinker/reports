#' Utility Fuction to Copy reports Package JavaScript
#' 
#' Copies all the reports Package JavaScript files to designated directory.
#' 
#' @param outfile The directory to copy the JavaScript files to.
#' @keywords javascript
#' @export
js_copy <- function(outfile = "PRESENTATION/assets/js") {
    js <- system.file("extdata/reports_js", package = "reports")
    suppressWarnings(file.copy(file.path(js, dir(js)), 
        outfile, recursive = TRUE))
    message(sprintf("javascript copied to %s", outfile))
}
