CITEhelper <- function(text.loc = NULL, from = "markdown", to = "latex",
    copy2clip = TRUE, citation = TRUE){
    if (is.null(text.loc)) {
        nts <- notes()
        cat("\n\n\bPlease select a row number from the entries above:\n\n")
        text.loc <- as.numeric(readLines(n=1))
    } else {
    	if (is.character(text.loc)) {
    	    nts <- notes(col.width=FALSE)
    	    nts <- nts[grepl(text.loc, nts[, "bibkey"], ignore.case=TRUE), ]
    	    print(truncdf(nts, end = 70))
            cat("\n\n\bPlease select a row number from the entries above:\n\n")
            text.loc <- as.numeric(readLines(n=1))
	
        } else {
            nts <- notes(col.width=FALSE)
        }
    }
    if(text.loc > nrow(nts)) stop("text.loc exceeds number of note entries")
    txt <- nts[text.loc, "quote"]
    out <- list(QC(to=to, from=from, text=txt, copy2clip = FALSE))
    if(citation) {
        out[["pgs"]] <- nts[text.loc, "page"]
        out[["bibkey"]] <- nts[text.loc, "bibkey"]
    }   
    return(out)
}