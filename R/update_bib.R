#' Update bib
#' 
#' Updates the report directory bib file with a global/master bib file.
#' 
#' @param report Path to the report project.
#' @param bib.loc Optional path to a .bib resource.
#' @return Updates bib from master/global bib
#' @import qdap
#' @export
update_bib <-
function(report = getwd(), bib.loc = getOption("bib.loc")) {
    x <- paste0(report, "/REPORT/")
    z <- paste0(report, "/PRESENTATION/")
    y <- dir(z)
    current <- y[tools::file_ext(y) == "bib"]
    current
    if (is.null(bib.loc)) stop("please supply the path to the .bib file")    
    new <- tail(unlist(strsplit(bib.loc, "/")), 1)
    if (current != new) {
        cat(paste0("Current .bib file does not match updated .bib.  Do you want to continue?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("update_bib aborted")
        }
    }
    suppressWarnings(try(file.copy(bib.loc, x, overwrite = TRUE)))
    invisible(file.copy(bib.loc, z, overwrite = TRUE))
    cat("bib files updated!")
}