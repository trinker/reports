#Helper function for html5/not exported
wheresPandoc <- function() {
    myPaths <- c("pandoc",  "~/.cabal/bin/pandoc", 
        "~/Library/Haskell/bin/pandoc", "C:\\PROGRA~1\\Pandoc\\bin\\pandoc")
    panloc <- Sys.which(myPaths)
    temp <- panloc[panloc != ""]
    if (identical(names(temp), character(0))) {
        ans <- readline("Pandoc not installed in one of the typical locations.\n 
            Do you know where Pandoc is installed? (y/n) ")
        if (ans == "y") {
        	temp <- readline("Enter the (unquoted) path to Pandoc: ")
        } else {
            if (ans == "n") {
            	stop("Pandoc not installed or not found.")
            }
        }
    } 
    temp
}