#' Quick Path
#' 
#' This function is a wrapper for \code{\link[base]{file.path}} and 
#' \code{\link[base]{getwd}}
#' 
#' @param \ldots The directory (or nested directories) within the working 
#' directry.
#' @return Returns a path.
#' @export
#' @seealso \code{\link[base]{file.path}},
#' \code{\link[base]{getwd}}
#' @examples
#' QP()
#' QP("PRESENTATION")
#' QP("PRESENTATION/figure")
#' QP("PRESENTATION", "figure")
QP <- 
function(...) {
    file.path(getwd(), ...)
}
