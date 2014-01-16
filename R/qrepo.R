#' Quick Repository Template
#' 
#' Generate a basic repository template ready for GitHub.
#' 
#' @param repo A character vector naming the repo.
#' @param github.user GitHub user name (character string).
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param open logical.  If \code{TRUE} the project will be opened in RStudio.  
#' The default is to test if \code{presentation} is being used in the global 
#' environment, if it is then the project directory will be opened.  
#' @param github logical.  If \code{TRUE} the repo will be sent to public 
#' \href{https://github.com/}{GitHub} account.
#' @return Creates a repository template.
#' @export
#' @examples
#' ## qrepo("New")
qrepo <- function(repo = "repo", github.user = getOption("github.user"), 
    path = getwd(), open = is.global(2), github = FALSE) {
	
    ## Replace spaces in repo name with underscore
    repo <- gsub("\\s+", "_", repo)

    ## Check if repo already exists; user inputs handling of conflicts
    if(file.exists(file.path(path, repo))) {
        message(paste0("\"", file.path(path, repo), 
            "\" already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("repo aborted")
        } else {
            delete(file.path(path, repo))
        }
    }

    ## Create main directory and change working directory 
    suppressWarnings(invisible(dir.create(file.path(path, repo),
        recursive = TRUE))) 
    x <- file.path(path, repo)
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)   

    ## reports extra data root firectory
    root <- system.file("extdata/present", package = "reports")

    ## Add notes, to do, extra_functions, and Rmd
    cat(file = file.path(x, "TO_DO"))
    cat(file = file.path(x, "NOTES"))
    date1 <- "### `r as.character(format(Sys.Date(), format=\"%B %d, %Y\"))`"
    cat(sprintf("# %s\n%s\n\n", repo, date1),
        file = file.path(x, paste0(repo, ".Rmd")))  
    cat("## Extra functions here", 
        file = file.path(x, "extra_functions.R"))

    ## Add .rproj
    invisible(file.copy(file.path(root, "TEMP.txt"), x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(repo, ".Rproj"))))

    ## Add .Rprofile
    rpro <- c("#Load the packages used",
        "library(reports); library(knitr)", "")  
    rpro2 <- c("", "#Source \"extra_functions.R\":",
        "source(file.path(getwd(), \"extra_functions.R\"))")

    if (!is.null(!is.null(github.user) && file.exists(github.user))) {
        git <- paste0("options(github.user = \"", github.user, "\")")
        rpro <- c(rpro, git)
    }       
    cat(paste(c(rpro, rpro2), collapse = "\n"), file = file.path(x, 
        ".Rprofile"))
  
    ## Add class
    o <- paste0("Repository \"", repo, "\" created:\n", x, "\n")
    class(o) <- "reports"
        
    ## Send to github
    if (github) {
    	try(repo2github(project.dir = x))
    }
    
    ## Open Project in RStudio
    if (open) {
        open_project(file.path(x, paste0(repo, ".Rproj")))
    }  
    
    return(o)    
}

