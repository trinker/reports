#' Recursive Directory Creation
#' 
#' Allows the user to input pieces of directory names to quickly generate 
#' multiple directories with similar names nested in the same directory.
#' 
#' @param \ldots The pieces of the names to put together.  \code{rdirs} will use
#' R's recylcing rule with different length vectors.
#' @param path A character vector specifying the root directory path.
#' @param sep  A character string to separate the terms.
#' @param pad.num logical.  If TRUE numbers will be padded with leading zeros 
#' (detects numeric strings supplied using the colon(\code{:}) operator or 
#' combine (\code{c(}) function.
#' @return Generates recursive sub directories.
#' @seealso  \code{\link[reports]{folder}}, 
#' \code{\link[base]{delete}}, 
#' \code{\link[base]{dir.create}}
#' @keywords file, directory, folder
#' @export
#' @examples
#' \dontrun{
#' rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6))
#' rdirs(session, 1:12, seq(as.Date("2000/1/1"), by = "month", length.out = 12))
#' }
rdirs <- function(..., path = getwd(), sep = "_", pad.num = TRUE) {
    pieces <- as.character(match.call(expand.dots = FALSE)[[2]])
    plist <- lapply(pieces, "[")
    nums <- grepl("[0-9][:]|[c(]", pieces)
    plist[nums] <- invisible(lapply(pieces[nums], function(x) {
        x <- eval(parse(text=x))
        if (pad.num) {
            x <- pad(x)
        }
        x
    }))
    nms <- paste2(plist, sep=sep)
    invisible(lapply(file.path(path, nms), dir.create))
    cat(paste0("directories create in: \n", path, "\n"))
}