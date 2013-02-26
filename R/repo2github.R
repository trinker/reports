#' Upload a Local Repo to GitHub (WARNING: CURRENTLY NOT FUNCTIONING)
#' 
#' (WARNING: CURRENTLY NOT FUNCTIONING)
#' 
#' Allows uploading a local repository to GitHub without first creating the 
#' repository in the clouds. 
#' 
#' @param user GitHub user name (character string).
#' @param password GitHub password (character string).
#' @param repo Character string naming the repo; default attempts to ude the 
#' report project directory name.
#' @param gitpath Path to the location of git.  If \code{NULL} 
#' \code{repo2github} will attempt to locate the path if necessary.
#' @return Creates GitHub reopository.
#' @author SimonO101 of stackoverflow.com and tyler Rinker <tyler.rinker@@gmail.com>
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
repo2github <- function(user = getOption("github.user"), password = "", 
	repo = basename(getwd()), gitpath = NULL) {
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
        cmd1 <- paste0(tempdir(), "/curl -u '", user, ":", password, 
            "' https://api.github.com/user/repos -d '{\"name\":\"", repo, "\"}'")
    }
    system(cmd1)   
#    system(paste(gitpath, "--version"))
    
    system(paste0(gitpath, " remote add origin git@github.com:", user, "/", 
        repo, ".git"))
    system(paste0(gitpath, " add NOTES.txt"))
    system(paste0(gitpath, " commit -m \"Initial commit.\""))
    system(paste0(gitpath, " push -u origin master"))
    cat("repo pushed to github\n")
}  