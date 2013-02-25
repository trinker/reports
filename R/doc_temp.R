#' Document Template
#' 
#' This function generates a document template for submission to the reports 
#' package to be inclusion as a template in the doc_library.  This function is 
#' similar to \code{\link[utils]{package.skeleton}}.
#' 
#' @param temp.name The name of the template.
#' @param doc.type The type of report that the template will contain 
#' (\code{"doc", "rnw" or "tex"}).  \code{"doc"} will contain a .docx document 
#' whereas \code{"rnw" and "tex"} contain both \code{doc.rnw}/\code{doc.tex} and 
#' \code{preamble.tex} files.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @details templates must contain the following two items:
#' \itemize{
#' \item{DESCRIPTION}{ - A file used to keep track of users and package 
#' information.  All fields must be filled in.}
#' \item{documents}{ - A minimal working document template.}
#' \itemize{
#' \item{If \code{doc.type = rnw}}{ - Must contain: doc.rnw (preamble included)}
#' \item{If \code{doc.type = tex}}{ - Must contain: doc.tex and preamble.tex}
#' \item{If \code{doc.type = docx}}{ - Must contain: doc.docx}
#' }
#' }
#' Additional project files and directores can be stored in the \code{inst}  
#' directory.Files in this directory will be placed in the main directoy of the 
#' templace created by \code{\link[reports]{new_report}}
#' 
#' To be submitted a .tex template must run in Debian TeXLive on Linux Mint and 
#' MikTex on Windows.  After a template has been tested it can be sent as a 
#' .zip file to \code{reports.rpackage@@gmail.com}.  Please note that the 
#' template folder/directory name must end in _tex, _rnw or _doc to indicate to 
#' users type of template.
#' @return Creates a document template framework for template personal use or 
#' submission.
#' @export
doc_temp <- 
function(temp.name = "newDoc", doc.type = "tex", path = getwd()) {
    tn <- paste0(temp.name, "_", doc.type)
    if(file.exists(file.path(path, tn))) {
        cat(paste0("\"", file.path(path, tn), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("doc_temp aborted")
        } else {
            delete(file.path(path, tn))
        }
    }   
    x <- folder(folder.name = tn)
    root <- system.file(paste0("extdata/doc_temp/", doc.type), 
        package = "reports")
    fls <- file.path(root, dir(root))
    invisible(file.copy(file.path(root, dir(root)), x))
    desc <- readLines(file.path(x, "DESCRIPTION"))
    desc[1:2] <- c(paste("Template:", temp.name), paste0(desc[2], Sys.Date()))
    cat(paste(desc, collapse="\n"), file=file.path(x, "DESCRIPTION"))
    folder(folder.name = file.path(tn, "inst"))
    cat(paste0("template created:\n", x, "\n"))
}