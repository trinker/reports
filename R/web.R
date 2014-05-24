#' Functions for Internet Articles/Information
#' 
#' The web family of functions enable quick reading, writing and browsing of 
#' Internet urls related to the project, all housed in a central location.
#' 
#' @param category An optional category subest to be assigned to 
#' (\code{write.web}) or selected from (\code{web}).  
#' @param select A integer row number from the 
#' \file{websites.xlsx}/\file{websites.csv}.  If this value is \code{NULL} an 
#' interactive selection will be enabled.
#' @param path Path to the websites.csv/websites.xlsx.  If 
#' \code{missing}/\code{NULL} the location will be assumed to be in 
#' \file{~/ARTICLES}.
#' @param \ldots Other arguments passed to \code{\link[utils]{read.csv}} or 
#' \code{\link[xlsx]{read.xlsx}}
#' @param url The url from the website to be entered into 
#' \file{websites.xlsx}/\file{websites.csv}.
#' @param description An optional description to be entered into 
#' \file{websites.xlsx}/\file{websites.csv}.
#' @param sheet The sheet name of the \file{websites.xlsx} file.  The defualt is
#' \code{"Sheet1"}.
#' @keywords website, Internet, url
#' @export
#' @rdname web
#' @importFrom xlsx read.xlsx
#' @importFrom tools file_ext
#' @importFrom XLConnect loadWorkbook appendWorksheet saveWorkbook	
web <- function(category = NULL, select = NULL, path = NULL) {
	
	## Read in the websites from websites.csv/websites.xlsx
    dat <- read.web(path = path)
	
	## Subset by category
	if (!is.null(category)) {
		dat <- dat[dat$category == category, ]
	}
	
	## Interactive selection if select is.null
    if (is.null(select)) {
        WD <- options()[["width"]]
        options(width=3000)    	
    	dat2 <- data.frame(dat, row.names = NULL)
        #print(truncdf(left.just(dat2), end = desc.width)) #removed 12-13-13
    	print(left.just(dat2))                             #added 12-13-13
    	message("===============\n\nEnter a rownumber from the frame above")
        select <- as.numeric(readLines(n=1))
        options(width=WD)
    }

	## open the website
	message("Preparing to open browser...")
    browseURL(as.character(dat[select, 1]))
	
}	

#' @export
#' @rdname web
read.web <- 
function(path, ...) {

    ## If missing path assume it's in ARTICLES
    if (missing(path) || is.null(path)) {
        fls <- dir("ARTICLES")
        isweb <- fls[grepl("websites\\.", fls)]
        path <- sprintf("ARTICLES/%s", isweb[1])        
    }

    ## Make sure pathe exists
    if (!file.exists(path)) {
        stop(sprintf(paste0("\nThe following path does not exist:\n\n  %s\n\n", 
            "Check your working directory"), path))
    }
       
    ## Read it in
    switch(file_ext(path),  
        xlsx = {x <- read.xlsx(path, 1, ...)},
        csv = {x <- read.csv(path, ...)},
        stop(sprintf(".%s is not currently supported", file_ext(path)))
    )
        
    ## Remove the example
        lens <- seq_len(nrow(x))
	  these <- lens[!lens %in% which(x[, "category"] == "example_reports")]
    dat <- data.frame(x[these, ], row.names = NULL)
    class(dat) <- c("read.web", class(dat))
    dat
}

#' Prints a read.web Object.
#' 
#' Prints a read.web object.
#' 
#' @param x The read.web object
#' @param \ldots ignored
#' @method print read.web
#' @export
print.read.web <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}

#' @export
#' @rdname web
write.web <- function(url, description=NA, category = NA, path, 
    sheet = "Sheet1", ...) {
	
    ## If missing path assume it's in ARTICLES
    if (missing(path)) {
        fls <- dir("ARTICLES")
        isweb <- fls[grepl("websites\\.", fls)]
        path <- sprintf("ARTICLES/%s", isweb[1])	
    }

    ## Make sure pathe exists
    if (!file.exists(path)) {
        stop(sprintf(paste0("\nThe following path does not exist:\n\n  %s\n\n", 
        "Check your working directory"), path))
    }
	
    dat <- data.frame(url = url, description = description, category = category)

    ## Read it in
    switch(file_ext(path), 
        xlsx = {wb <- loadWorkbook(path)
            appendWorksheet(wb, dat, sheet = sheet)
            saveWorkbook(wb)},
        csv = {cn <- ifelse(append, FALSE, TRUE) 
            write.table(dat, path, append = TRUE, ..., row.names = FALSE,  
            sep = ",",  qmethod = "double", col.names = cn)},
        stop(sprintf(".%s is not currently supported", file_ext(path)))
    )
    message("url has been saved")
}

