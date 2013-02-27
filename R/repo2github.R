#' Upload a Local Repo to GitHub (WARNING: CURRENTLY NOT FUNCTIONING)
#' 
#' (WARNING: CURRENTLY NOT FUNCTIONING)
#' 
#' Allows uploading a local repository to GitHub without first creating the 
#' repository in the clouds. 
#' 
#' @param password GitHub user password (character string).
#' @param project.dir The path to the root directory of the report/presentation.
#' @param repo Character string naming the repo; default attempts to use the 
#' report project directory name.
#' @param user GitHub user name (character string).
#' @param gitpath Path to the location of git.  If \code{NULL} 
#' \code{repo2github} will attempt to locate the path if necessary.
#' @return Creates GitHub repository.
#' @author Simon O'Hanlon and Tyler Rinker <tyler.rinker@@gmail.com>
#' @references \url{http://stackoverflow.com/a/15047013/1000343} 
#' @section GitHub Website: \url{https://github.com/}
#' @note To use \code{repo2github} the user must have initiallized 
#' \code{\link[reports]{new_report}} or \code{\link[reports]{presentation}} 
#' with a \code{.git} file by selecting the argument \code{git = TRUE}.  The 
#' user will also need to have a \href{https://github.com/}{GitHub} site 
#' established.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{github.user} in the user's primary \code{.Rprofile}.
#' @export
#' @examples
#' \dontrun{
#' repo2github()
#' }
repo2github <- function(password, project.dir = getwd(), 
	repo = basename(getwd()), user = getOption("github.user"), gitpath = NULL) {
#CReates the repo (this works)
    if (Sys.info()["sysname"] != "Windows") {
        gitpath <- "git"
        cmd1 <- paste0("curl -u '", user, ":", password, 
            "' https://api.github.com/user/repos -d '{\"name\":\"", repo, "\"}'")
    } else {
        if (is.null(gitpath)){  
            test <- c(file.exists("C:\\Program Files (x86)\\Git\\bin\\git.exe"),
                file.exists("C:\\Program Files\\Git\\bin\\git.exe"))
            if (sum(test) == 0) {
                stop("Git not found.  Supply path to 'gitpath'")    
            }
            gitpath <- c("\"C:\\Program Files (x86)\\Git\\bin\\git\"",
                "\"C:\\Program Files\\Git\\bin\\git\"")[test][1]
        }
        url <- "http://curl.askapache.com/download/curl-7.23.1-win64-ssl-sspi.zip"
        tmp <- tempfile( fileext = ".zip" )
        download.file(url,tmp)
        unzip(tmp, exdir = tempdir())       
        system(paste0(tempdir(), "/curl http://curl.haxx.se/ca/cacert.pem -o " , 
            tempdir() , "/curl-ca-bundle.crt"))
     	json <- paste0(" { \"name\":\"" , repo , "\" } ") #string we desire formatting
	    json <- shQuote(json , type = "cmd" )
        cmd1 <- paste0( tempdir() ,"/curl -i -u \"" , user , ":" , password , 
            "\" https://api.github.com/user/repos -d " , json )
	
    }
    system(cmd1)   
#Now to push the directory to github (does not work)
    
#git commands needed:
# git remote add origin git@github.com:USER/REPO.git
# git push origin master
#
#I thought I could do:     
#system(paste0(gitpath, " remote add origin git://github.com:", user, "/", repo, ".git"))    
    
    cat("repo pushed to github\n")
}  