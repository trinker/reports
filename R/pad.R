#' Pad Strings
#' 
#' A convenience wrapper for \code{\link[base]{sprintf}} that pads the front end 
#' of strings with spaces or 0s. Useful for creating multiple unifor directories 
#' that will maintain correct order.
#' 
#' @param x A character, factor, numeric vector.
#' @param padding Number of characters to pad.  Default makes all elements of a 
#' string the number of characters of the element with the maximun characters.
#' @param sort logical.  If \code{TRUE} the outcome is sorted.
#' @param type A character string of \code{"detect"}, \code{"numeric"}, 
#' \code{"character"}, \code{"d"} or \code{"s"}.  If numeric zeros are padded.  
#' If character spaces are padded.  The \code{detect} attempts to determine if x 
#' is numeric (d) or not (s).
#' @return Returns a charcter vector every element padded with 0/spaces.
#' @export
#' @seealso \code{\link[base]{sprintf}}
#' @examples
#' pad(sample(1:10, 6))
#' pad(sample(1:10, 6), sort=FALSE)
#' pad(as.character(sample(1:10, 6)))
#' pad(as.character(sample(1:10, 6)), 4)
pad <- function(x, padding=max(nchar(as.character(x))), sort = TRUE, type="detect") {
    poss <- c("detect", "numeric", "character", "d", "s")
    if (!type %in% poss) stop("type must be: \"detect\", \"numeric\"\\\"d\" or \"character\"\\\"s\"")
    Rel <- c(NA, "d", "s", "d", "s")
    type <- Rel[poss %in% type]
    if (is.na(type)) {
        type <- ifelse(is.numeric(x), "d", "s")
    }
    x <- sprintf(paste0("%0", padding, type), x)
    if (sort) {
        x <- sort(x)
    }
    x
}