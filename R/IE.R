#' Flexible ifelse Like Functionality for Arguments
#' 
#' Use in the place of \code{ifelse} for arguments (particularly if \code{NULL} 
#' is an option).
#' 
#' @param input An object to be tested. 
#' @param out1 The output if test is \code{TRUE}.
#' @param out2 The output if test is \code{FALSE}.
#' @param FUN A function that results in a logical output. 
#' @return Returns output 1 (out1) or output 2 (out2) as a function of the logical 
#' test. 
#' @export
#' @examples
#' IE(25, 360)
#' IE("char", 360)
#' IE(NULL, 360)
#' IE(NULL, 360, FUN = is.null)
IE <- function(input, out1, out2 = NULL, FUN  = is.numeric) {
    if (match.fun(FUN)(input)) {
        out1
    } else {
        out2
    }
}
