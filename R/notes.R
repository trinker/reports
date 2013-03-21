#' View a Notes
#' 
#' Allos a truncated view of your notes (main_dir/ARTICLES/notes).
#' 
#' @param col.width An integer value of the maximum width of columns.
#' @return Returns a truncated view of user notes.
#' @export
#' @import gdata
#' @examples 
#' \dontrun{
#' notes()
#' }
notes <- function(col.width =70) {
    read.notes(trunc = col.width)	
}