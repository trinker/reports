# NOT CURRENTLY EXPORTED OR USED
#
# Upload a Local Repo to GitHub (WARNING: CURRENTLY NOT FUNCTIONING)
# 
# (WARNING: CURRENTLY NOT FUNCTIONING)
# 
# Allows uploading a local repository to GitHub without first creating the 
# repository in the clouds. 
# 
# @param password GitHub user password (character string).
# @param project.dir The path to the root directory of the report/presentation.
# @param repo A character string naming the repo; default attempts to use the 
# report project directory name.
# @param github.user GitHub user name (character string).
# @param gitpath Path to the location of git.  If \code{NULL} 
# \code{repo2github} will attempt to locate the path if necessary.
# @return Creates GitHub repository.
# @author Simon O'Hanlon and Tyler Rinker <tyler.rinker@@gmail.com>
# @references \url{http://stackoverflow.com/a/15047013/1000343} 
# @section GitHub Website: \url{https://github.com/}
# @section Warning: The arguments \code{project.dir} and \code{repo} use 
# \code{\link[base]{getwd}}.  This assumes is the current working directoy is 
# the root directory and is done for convienence.  The user should ensure that 
# either their working directory is the root directory or supply the correct 
# root directory/name to these arguments.
# @note To use \code{repo2github} the user must have initialized 
# \code{\link[reports]{new_report}} or \code{\link[reports]{presentation}} 
# with a \code{.git} file by selecting the argument \code{git = TRUE}.  The 
# user will also need to have a \href{https://github.com/}{GitHub} site 
# established.
# @section Suggestion: The user may want to set \code{\link[base]{options}} for 
# \code{github.user} in the user's primary \code{.Rprofile}.
# @export
# @examples
# \dontrun{
# repo2github()
# }
repo2github <- function(password, project.dir = getwd(), 
	repo = basename(getwd()), github.user = getOption("github.user"), 
	gitpath = NULL) {

    #check for password
    if (missing(password)) {	
        cat ("Enter [GitHub password] to continue\n")
        password <- readLines(n=1)
    }
    
    OSiswin <- Sys.info()["sysname"] != "Windows"
    
    #Create the repo
    if (OSiswin) {
        gitpath <- "git"
        cmd1 <- paste0("curl -u '", github.user, ":", password, 
            "' https://api.github.com/user/repos -d '{\"name\":\"", repo, "\"}'")
    } else {
        if (is.null(gitpath)){  
            test <- c(file.exists("C:/Program Files (x86)/Git/bin/git.exe"),
                file.exists("C:/Program Files/Git/bin/git.exe"))
            if (sum(test) == 0) {
                stop("Git not found.  Supply path to 'gitpath'")    
            }
            gitpath <- c("C:/Program Files (x86)/Git/bin/git.exe",
                "C:/Program Files/Git/bin/git.exe")[test][1]
        }
        url <- "http://curl.askapache.com/download/curl-7.23.1-win64-ssl-sspi.zip"
        tmp <- tempfile( fileext = ".zip" )
        download.file(url,tmp)
        unzip(tmp, exdir = tempdir())       
        system(paste0(tempdir(), "/curl http://curl.haxx.se/ca/cacert.pem -o " , 
            tempdir() , "/curl-ca-bundle.crt"))
     	json <- paste0(" { \"name\":\"" , repo , "\" } ") #string we desire formatting
	    json <- shQuote(json , type = "cmd" )
        cmd1 <- paste0( tempdir() ,"/curl -i -u \"" , github.user , ":" , password , 
            "\" https://api.github.com/user/repos -d " , json )
	
    }
    system(cmd1)  
    #Now to push the directory to github
    #be careful that github.user is correct or git will get messed up
    #could probably do with some references to how git will get confused and how to solve it
    if (is.null(project.dir)) stop("\"project.dir\" must be supplied")
    
    if (!OSiswin) {    

        wd <- getwd()
        setwd(project.dir)
        cmd2 <- paste0(shQuote(gitpath), " init")
        system(cmd2, intern = T)
        cmd3 <- paste0(shQuote(gitpath), " add -A")  ## maybe cmd3 <- paste0(shQuote(gitpath), " add .")
        system(cmd3, intern = T)       

        ## Set email
        x <- file.path(path.expand("~"), ".gitconfig")
        if (file.exists(x)) {
            y <- readLines(x)
            email <- Trim(unlist(strsplit(y[grepl("email = ", y)], "email ="))[2])
        } else {
            z <- file.path(Sys.getenv("HOME"), ".gitconfig")
            if (file.exists(z)) {
                email <- Trim(unlist(strsplit(y[grepl("email = ", y)], "email ="))[2])
            } else {
                warning(paste("Set `email` in", x))
            }
        }
        cmdEM <- paste0(shQuote(gitpath), sprintf(" config --global user.email %s", email))        
        system(cmdEM)
        
        ## Initial commit
        cmd4 <- paste0(shQuote(gitpath), ' commit -a -m "Initial commit"')  
        system(cmd4, intern = T) 

        
        ## system(paste(shQuote(gitpath), "remote rm origin"))
        
        ##
        ## cmd5 <- paste(shQuote(gitpath), sprintf(paste("remote add", repo, "git@github.com:%s/%s.git"), github.user, repo))
        ## system(cmd5, intern = T) 
        
        ## 
        cmd5 <- paste0(shQuote(gitpath), " remote add origin https://github.com:",
            github.user, "/", repo, ".git")  
        system(cmd5, intern = T) 
        
        ## 
        cmd6 <- paste0(shQuote(gitpath), " push -u origin master")  
        system(cmd6, intern = T) 
        
        setwd(wd)	
    } else {
        system( paste0( "cd ", project.dir , " && " , gitpath , " init" ) )
        system( paste0( "cd ", project.dir , " && " , gitpath , " add \\." ) )
        system( paste0( "cd ", project.dir , " && " , gitpath , 
            " commit -m \"Initial commit\"" ) )
        system( paste0( "cd ", project.dir , " && " , gitpath, 
            " remote add origin https://github.com:", github.user, "/", repo, ".git") ) 
    }
    message(paste(repo, "pushed to github\n"))
}  

#  password <-"pass"; project.dir = getwd(); repo = basename(getwd()); github.user = getOption("github.user"); Trim <- reports:::Trim
