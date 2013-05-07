#' View Bibliography
#' 
#' Allows a truncated view of your .bib.
#' 
#' @param bib Path to bibliography.  If \code{NULL} \code{BV} attempts to locate 
#' the .bib file (working directory first and then "main_dir/REPORTS/_.bib", 
#' last "main_dir/PRESENTATION/_.bib" in that order).
#' @param col.width An integer value of the maximum width of columns.
#' @return Returns a truncated view of user notes.
#' @export
#' @import knitcitations
#' @examples 
#' \dontrun{
#' notes()
#' }
BV <-
function(bib = NULL, col.width = 40) {
    loc1 <- getwd()
    loc2 <- file.path(loc1, "REPORT")
    loc3 <- file.path(loc1, "PRESENTATION")
    locs <- list(loc1, loc2, loc3)    	    	
    	
    FUN <- function(x) {
    	fls <- dir(x)  
        if (identical(fls, character(0))) return(NULL) 
    	fls[tools::file_ext(fls) == "bib"][1]
    }
    check <-lapply(locs, FUN)
    mark <- which(!sapply(check, is.null))[1]
    bibloc <- file.path(unlist(locs[mark]), FUN(unlist(locs[mark])))
    bibin <- suppressMessages(suppressWarnings(invisible(read.bibtex(bibloc))))
    title <- sapply(bibin, function(x) {
        tryCatch(clean(unlist(x)[grepl("\\.title", names(unlist(x)))]), 
            error=function(err) "unknown")   
    })
    truncdf(left.just(data.frame(num = 1:length(bibin), 
        bibkey = names(bibin), title = title, row.names = NULL), 2:3), col.width)
}
