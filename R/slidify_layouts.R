#' Generate Extra Slidify Layouts 
#' 
#' A collection of additional slidify slide layouts to extend the slidify 
#' framework.
#' 
#' @param path The path to the layout directory where the html files should be 
#' generated.  Default is \file{~assets/layouts}.
#' 
#' @return Creates the layout html files to extend the slidify slide layout 
#' framework.  Currently \code{slidify_layouts} generates a thankyou.html and 
#' youtube.html.  See the references for additional information.
#' @note If \code{\link[reports]{new_report}} or 
#' \code{\link[reports]{presentation}} was utilized to generate the slidify 
#' presentation directory \code{\link[reports]{slidify_layouts}} has already 
#' created the extra slidify slide layouts in: \file{~PRESENTATION/assets/layouts}
#' @references INSERT LINK TO slidify tricks and tips section
#' @export
slidify_layouts <- function(path = QP("assets/layouts")){
    root <- system.file("extdata/slidify_layouts", package = "reports")
    if (!file.exists(path)) stop("Supply a valid path argument")
    invisible(file.copy(file.path(root, dir(root)), path))
    message(paste0("The following files have been generated ", ":\n\n",
        paste(file.path(path, dir(root)), collapse="\n"), 
        "\n", paste(rep("=", 50), collapse=""),
    	"\n\nSee slidify tricks and tips section for more:\n\nINSERT LINK HERE\n"))
}

