#' Easy File Handling
#' 
#' \code{delete} - Deletes files and directories.
#' 
#' @param file The name of the file in the working directory or the path to the 
#' file to be deleted.  If \code{NULL} provides a menu of files from the working 
#' directory.
#' @param \ldots The name(s) of the folder to be created.  If both \ldots and
#' \code{folder.name} are \code{NULL} creates a file in the working directory 
#' with the creation date and time stamp.
#' @param folder.name A character vector of the name(s) of the folder to be 
#' created.  Default \code{NULL}  (if the \ldots  is \code{NULL} too) creates a 
#' file in the working directory with the creation date and time stamp.  Use 
#' this argument only if the directory names contain spaces.
#' @return \code{delete} permanently removes a file/directory.
#' @seealso  \code{\link[base]{unlink}}, 
#' \code{\link[base]{file.remove}}, 
#' \code{\link[base]{dir.create}}
#' @keywords file, delete, folder
#' @rdname file_handling
#' @export
#' @examples
#' \dontrun{
#' (x <- folder("DELETE.ME"))
#' which(dir() == "DELETE.ME")
#' delete("DELETE.ME")
#' which(dir() == "DELETE.ME")
#' 
#' folder("the/next/big/thing", "hello world", "now/is/the/time")
#' 
#' folder(cat, dog)
#' lapply(c("cat", "dog"), delete)
#' }
delete <-
function(file = NULL) {
    x <- if (is.null(file)) {
        menu(dir())
    } else {
        file
    }
    unlink(x, recursive = TRUE, force = FALSE)
}

#' Create Folder
#' 
#' \code{folder} - Create a folder/directory.
#' 
#' @return \code{folder} creates a folder/directory.
#' @rdname file_handling
#' @export
folder <- function(..., folder.name = NULL) {
    if (!is.null(folder.name)) {
        x <- strsplit(folder.name, split = ", ")
    } else {
        x <- substitute(...())
    }
    if (!is.null(x)) {
        x <- unblanker(scrubber(unlist(lapply(x, function(y) {
            as.character(y)}))))
    }
    if (is.null(x)) {
        hfolder()
    } else {
        if (length(x) == 1) {
            hfolder(x)
        } else {
            lapply(x, function(z) {
                hfolder(z)
            })
        }
    }
}

hfolder <- function(folder.name = NULL) {
    if (is.null(folder.name)) {
        FN <- mgsub(c(":", " "), c(".", "_"), 
            substr(Sys.time(), 1, 19))
    } else {
        FN <-folder.name
    }
    parts <- unlist(strsplit(FN, "/"))
    if (length(parts) == 1) {
        x <- paste(getwd(), "/", FN, sep = "")
    } else {

        ## If nested path (multiple directories created)
        if (!file.exists(dirname(FN))) {

            y <- FN
            z <- length(parts)
            for (i in rev(seq_along(parts))) {
                if(file.exists(y)) {
                    z <- z + 1
                    break
                }
                y <- dirname(paste(parts[1:i], collapse ="/"))
                z <- z - 1
            }
            
            for (i in z:(length(parts) - 1)) {
                suppressWarnings(dir.create(paste(parts[1:i], collapse ="/")))
            }
        
        }
        x <- FN
    }
    dir.create(x)
    return(x)
}
