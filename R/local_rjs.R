#' View reveal.js Slide Notes Locally 
#' 
#' Reconfigures the HTML output of a slidify reveal.js presentation for use with 
#' a reveal.js full install.  This allows the user to view slide notes without 
#' an Internet connection (i.e., local host is used as the server).
#' 
#' @param reveal.html The location of the 
#' \href{https://github.com/hakimel/reveal.js/}{reveal.js} created html document.  
#' Default \code{NULL} will use the first available html file in the 
#' PRESENTATION directory.
#' @param revealjs.loc
#' @details slidify generated reveal.js presentations are not configured for use 
#' with a full install revealjs.  This means that slide notes are not available 
#' for local use.  \code{local_rjs} reconfigures the html output and optionally 
#' places the file in the user's reveal.js full install directory.  Additionally 
#' the style.css from reports is copied to the css directory in the reveal.js 
#' full install and named reports.css.
#' 
#' For maximum efficiency configure the \code{\link[base]{options}} for 
#' \code{reveraljs.loc} in their \code{.Rprofile}.
#' @seealso \href{https://github.com/hakimel/reveal.js/}{Installation section of reveal.js GitHub}
#' @export
#' @examples 
#' ## local_rjs()
local_rjs <- function(reveal.html = NULL, 
    revealjs.loc = getOption("revealjs.loc")) {
    if (is.null(reveal.html)) {
        loc <- file.path(getwd(), "PRESENTATION")
        fl <- dir(loc)[tools::file_ext(dir(loc)) == "html"][1]
        reveal.html <- file.path(loc, fl)
    }
    if (!file.exists(reveal.html)){
        stop("reveal.html does not exist")	
    }
    html <- suppressWarnings(readLines(reveal.html))
    root <- system.file("extdata/docs", package = "reports")	
    htmlrep <- suppressWarnings(readLines(file.path(root, "reveal.js_head_foot.txt")))
    mid <- which(grepl("BOTTOM", htmlrep))
    html <- html[(1 +which( grepl("</head>", html))):length(html)]
    html <- html[1:tail(which(grepl("</section>", html)), 1)]
    out <- c(htmlrep[1:(mid - 1)], html, htmlrep[(mid + 1):length(htmlrep)])
    out <- paste(out, collapse="\n")
    if (!is.null(revealjs.loc)) {
        repsty <- file.path(revealjs.loc, "css/reports.css")
        if (!file.exists(repsty)) {
            css <- suppressWarnings(readLines(file.path(root, "style.css")))
            cat(paste(css, collapse="\n"), file= repsty)
        }
        cat(out, file=file.path(revealjs.loc, fl))
        mess <- c("Run in CMD:\n", paste("1. cd", revealjs.loc), 
            "2. grunt serve\n", "And then in your browser...\n",
            paste0("3. http://localhost:8000/", fl))
        message(paste(mess, collapse="\n"))
    } else {
        part2 <- paste0(tools::file_path_sans_ext(fl), "2.html")  
        outfl <- file.path(dirname(reveal.html), part2)
        cat(out, file=outfl)
        message(paste0("Output to:\n\n", outfl))
    }
}



