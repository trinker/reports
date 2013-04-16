#' View Notes
#' 
#' Allows a truncated view of your notes (main_dir/ARTICLES/notes).
#' 
#' @param col.width An integer value of the maximum width of columns.
#' @param notes.col logical,  If TRUE the notes column will be displayed.
#' @return Returns a truncated view of user notes.
#' @export
#' @import XLConnect
#' @examples 
#' \dontrun{
#' notes()
#' }
notes <- function(col.width = 70, notes.col = TRUE) {
    mess <- paste0("\n\nPossible causes:\n\n",
    "1) The main file directory is not set as the working directory\n",
    "2) ~/ARTICLES/notes.xlsx or ~/ARTICLES/notes.csv does not exist")
    out <- tryCatch(read.notes(trunc = col.width, notes.col = notes.col, print=FALSE), 
        error=function(err) stop(mess))
    out
}

notes2 <- function(col.width = 70, notes.col = TRUE) {
    mess <- paste0("\n\nPossible causes:\n\n",
    "1) The main file directory is not set as the working directory\n",
    "2) ~/ARTICLES/notes.xlsx or ~/ARTICLES/notes.csv does not exist")
    out <- tryCatch(read.notes(trunc = col.width, notes.col = notes.col), 
        error=function(err) stop(mess))
    out
}