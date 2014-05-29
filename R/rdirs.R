#' Recursive Directory Creation
#' 
#' Allows the user to input pieces of directory names to quickly generate 
#' multiple sub-directories with similar names nested in the same directory.
#' 
#' @param \ldots The pieces of the names to put together.  \code{rdirs} will use
#' R's recylcing rule with different length vectors.
#' @param path A character vector specifying the root directory path.
#' @param sep  A character string to separate the terms.
#' @param pad.num logical.  If \code{TRUE} numbers will be padded with leading 
#' zeros (detects numeric strings supplied using the colon(\code{:}) operator or 
#' combine (\code{c(}) function.
#' @param text.only logical.  If \code{TRUE} rdirs does not create the 
#' directories, but only returns the names.  This allows the names to be passed 
#' to \code{new_report} and \code{presentation}.
#' @return Generates recursive sub directories.  Invisibly returns the names of
#' the sub-directories.
#' @seealso  \code{\link[reports]{folder}}, 
#' \code{delete}, 
#' \code{\link[base]{dir.create}}
#' @keywords file, directory, folder
#' @export
#' @importFrom qdapTools pad
#' @examples
#' ## fx <- folder(delete_me)
#' ## owd <- getwd(); setwd(fx)
#' ## rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6))
#' rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
#' ## rdirs(session, 1:12, seq(as.Date("2000/1/1"), by = "month", length.out = 12))
#' 
#' x <- rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
#' ## lapply(x, new_report)
#' ## setwd(owd); delete(fx)
rdirs <- function(..., path = getwd(), sep = "_", pad.num = TRUE, 
    text.only = FALSE) {
    pieces <- as.character(match.call(expand.dots = FALSE)[[2]])
    plist <- lapply(pieces, "[")
    nums <- grepl("[0-9][:]|[c][\\(]|[qcv][\\(]", pieces)
    plist[nums] <- invisible(lapply(pieces[nums], function(x) {
        x <- eval(parse(text=x))
        if (pad.num) {
            x <- pad(x, sort = FALSE)
        }
        x
    }))
    nms <- paste2(plist, sep=sep)
    if (!text.only) {
        invisible(lapply(file.path(path, nms), dir.create))
        message(paste0("directories create in: \n", path, "\n"))
        invisible(nms)
    } else {
        return(nms)
    }
}
