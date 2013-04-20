#' File Syncing
#' 
#' \code{sync} - Sync files between directories.   
#' 
#' @param dir1 The path to the first directory.
#' @param dir2 The path to the second directory.
#' @param one_way logical.  If TRUE the contents of dir2 be copied to dir1, 
#' however, dir2 contents will not change.  If FALSE both dir1 and dir2 files 
#' will be copied to other (dir1 and dir2 will contain identical contents).
#' @param recursive logical. Should directories and contents be recursively 
#' copied?
#' @param \ldots For \code{sync} \ldots  are other arguments passed to 
#' \code{\link[base]{file.copy}}.  For \code{sync_img} and \code{sync_rnp} 
#' \ldots  are additional arguments passed to \code{sync}.  
#' @rdname sync
#' @return Syncs files between directories.
#' @seealso 
#' \code{\link[base]{file.copy}}
#' @export
sync <- function(dir1, dir2, one_way = FALSE, recursive = TRUE, ...) {
    a <- dir(dir1)
    b <- dir(dir2)
    a_unique <- setdiff(a, b)
    invisible(lapply(file.path(dir2, b_unique), function(x) {
        file.copy(x, dir1, recursive = recursive, ...)
    }))
    if (!one_way) {
        b_unique <- setdiff(b, a)
        invisible(lapply(file.path(dir1, a_unique), function(x) {
            file.copy(x, dir2, recursive = recursive, ...)
        }))
        message(paste(dir1, "and\n", dir2, "synced\n"))
    } else {
        message(paste(paste0(dir2, "'s"), "\ncontent(s) have been moved to", 
            paste0("\n", dir1), "\n"))
    }
}


#' Sync Images
#' 
#' \code{sync_img} - A wrapper for sync to easily sync the files in 
#' ~/PRESENTATION/figure and ~/PRESENTATION/assets/img.
#' 
#' @rdname sync
#' @export
sync_img <- function(dir1 = file.path(getwd(), "PRESENTATION", "figure"), 
    dir2 = file.path(getwd(), "PRESENTATION", "assets", "img"), ...) {
    sync(dir1 = dir1, dir2 = dir2, ...)
}

#' Sync Images2
#' 
#' \code{sync_rnp} - A wrapper for sync to easily sync the files in 
#' ~/REPORT/figure and ~/PRESENTATION/figure.
#' 
#' @rdname sync
#' @export
sync_rnp <- function(dir1 = file.path(getwd(), "REPORT", "figure"), 
    dir2 = file.path(getwd(), "PRESENTATION", "figure"), ...) {
    sync(dir1 = dir1, dir2 = dir2, ...)
}

