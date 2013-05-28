#' Report Template
#' 
#' Generate a report/paper template to increase efficiency.
#' 
#' @param report A character vector of length two or one: (1) the main directory 
#' name and (2) sub directory names (i.e., all the file contents will be 
#' imprinted with this name). If the length of \code{report} is one this name 
#' will be used as the main directory name and all sub directories and files.
#' @param template A character string of the internal reports template or an 
#' external path to a template in the reports package style.  This argument 
#' allows the user to change the contents of the report directory that is 
#' generated. See \code{templates} for more.
#' @param bib.loc Optional path to a .bib resource.
#' @param path The path to where the project should be created.  Default is the 
#' current working directory.
#' @param name A character string of the user's name to be used on the report.
#' @param github.user GitHub user name (character string).
#' @param sources A vector of path(s) to other scripts to be sourced in the 
#' report project upon startup (adds this location to the report project's 
#' \code{.Rprofile}).
#' @param AN.xlsx logical.  If \code{TRUE} the article notes (AN) will be in 
#' .xlsx format.  If \code{FALSE} the document will be a .csv file.
#' @param slidify  The template to be used in the PRESENTATION .Rmd.  This can 
#' be one of the types from \code{slidify_templates} or a path to an .Rmd file.  
#' This argument will be overrode if a custom reports template is supplied with 
#' an .Rmd file in the inst directory named slidify.Rmd (\code{/inst/slidify.Rmd}).
#' @param \ldots Other arguments passed to \code{\link[slidify]{author}}.
#' @section Suggestion: The user may want to set \code{\link[base]{options}} for 
#' \code{bib.loc}, \code{name.reports}, \code{temp.reports}, \code{github.user} 
#' and \code{sources.reports} in the user's primary \code{.Rprofile}:
#' \enumerate{ 
#'   \item{\bold{bib.loc} - The path to the users primary bibliography}
#'   \item{\bold{name.reports} - The name to use on reports}
#'   \item{\bold{temp.reports} - The primary template to use to generate reports 
#'   (see \code{template})}
#'   \item{\bold{github.user} - GitHub user name}
#'   \item{\bold{speed.temp} - A speed dial like interface that allows the 
#'   template argument to ake a numeric arguement.  Setting this option takes 
#'   the form of:\cr \code{options(speed.temp=list(`1`="wordpress_rmd", `2`="basic_rmd"))}}
#'   \item{\bold{sources.reports} - Path(s) to additional files/scripts that 
#'   should be included to be sourced in the project startup}
#'   \item{\bold{slidify.template} - Path to, or defualt, .Rmd file tempalte for 
#'   use in as the .Rmd used in the slidify presentations (see 
#'   \code{slidify_templates} for possible non-path arguments)}   
#' }
#' @return Creates a report template.
#' @seealso \code{\link[reports]{doc_temp}},
#' \code{\link[reports]{presentation}},
#' \code{\link[reports]{templates}},
#' \code{\link[reports]{slidify_templates}},
#' \code{\link[slidify]{author}}
#' @section Additional Guide: Introductory video
#' \url{http://youtu.be/cokNUTGtoM4}
#' @references 
#' \href{https://github.com/ramnathv/slidifyExamples/tree/gh-pages/examples}{slidify examples}
#' @export
#' @import slidify
#' @examples 
#' ## new_report()
new_report <-
function(report = "report", template = getOption("temp.reports"), 
    bib.loc = getOption("bib.loc"), name = getOption("name.reports"), 
    github.user = getOption("github.user"), 
    sources = getOption("sources.reports"), path = getwd(), AN.xlsx = TRUE, 
    slidify = getOption("slidify.template"), ...) {

    if (is.null(template)) template <- "apa6.mod.quant_rnw"
    if (is.numeric(template)) {
        spdTmp <- getOption("speed.temp")
        template <-  unlist(spdTmp[template == names(spdTmp)])
    }     
    if (!tail(unlist(strsplit(template, "_")), 1) %in% c("rnw", "rmd", "doc", "tex")) {
        stop("Please supply a correct template name")
    }
    report <- gsub("\\s+", "_", report)
    main <- head(report, 1)     
    report <- tail(report, 1)
    if(file.exists(file.path(path, main))) {
        cat(paste0("\"", file.path(path, main), 
            "\" already exists:\nDo you want to overwrite?\n\n"))
        ans <- menu(c("Yes", "No")) 
        if (ans == "2") {
            stop("new_report aborted")
        } else {
            delete(file.path(path, main))
        }
    }
    suppressWarnings(invisible(dir.create(file.path(path, main),
        recursive = TRUE))) 
    x <- file.path(path, main)
    REPORT <- ARTICLES <- OUTLINE <- PRESENTATION <- NULL
    WD <- getwd(); on.exit(setwd(WD))
    setwd(x)
    root <- system.file("extdata/doc_library", package = "reports")
    if (!template %in% dir(root)) {
        if (!file.exists(root)) {
            stop("template is not a valid reports package template or path")
        } else {
            if (template %in% dir(path)) {
                template <- file.path(path, template)
            } 
            root <- dirname(template)
            template <- basename(template)
        }        
    }
    if (!is.null(slidify) && slidify %in% dir(path)) {
        slidify <- file.path(path, slidify)   
    } 
    root2 <- system.file("extdata/docs", package = "reports")
    pdfloc <- file.path(root, template)
    desc <- suppressWarnings(readLines(file.path(pdfloc, "DESCRIPTION"))) #read in the description file
    desc.chunk1 <- "Outline:"
    desc.chunk2 <- "Presentation:"
    outline.type <- Trim(unlist(strsplit(gsub(desc.chunk1, "", desc[grepl(desc.chunk1, desc)]), ",")))   
    #outline.type <- gsub("rmd", "web", outline.type)
    pres <- Trim(unlist(strsplit(gsub(desc.chunk2, "", desc[grepl(desc.chunk2, desc)]), ","))) 
    type <- tail(unlist(strsplit(basename(template), "_")), 1)
    dat <- data.frame(inf=c("doc", "rnw", "tex", "rmd"), outf=c(1, 2, 2, 3), 
        stringsAsFactors = FALSE)
    if (all(!dat[, 1] %in% type)) {
        stop("template must end in \"_doc\", \"_tex\", \"_rnw\", or \"_rmd\"")
    }
    if (sum(pres %in% "rmd") < 1) {
        y <- invisible(folder(REPORT, ARTICLES, OUTLINE, PRESENTATION))
    } else {
        if (is.null(slidify)) {
            slidify <- "io2012"
        }
        y <- invisible(folder(REPORT, ARTICLES, OUTLINE))
        y[[4]] <- file.path(x, "PRESENTATION")
        suppressMessages(author(y[[4]], use_git = FALSE, open_rmd = FALSE, ...))
        if ("slidify.Rmd" %in% dir(file.path(pdfloc, "inst"))) {
            slid.path <- file.path(pdfloc, "inst/slidify.Rmd")
        } else {
            if(file.exists(slidify)) {
                slid.path <- slidify
            } else {
                if (slidify == "default") {
                    slid.path <- file.path(y[[4]], "index.Rmd")
                } else {
                    slid <- system.file("extdata/slidify_library", package = "reports")
                    if (substring(slidify, 1, 1) == ".") {
                        back <- "full"
                        slidify <- substring(slidify, 2)                        
                    } else {
                        back <- "min"
                    }
                    slid.path <- file.path(slid, back, paste0(slidify, ".Rmd"))
                }
            }
        }
        setwd(x)
        Rmd <- suppressWarnings(readLines(slid.path)) 
        title. <- grepl("title", Rmd) & grepl("\\:", Rmd) & !grepl("subtitle", Rmd)
        Rmd[title.] <- paste0("title      : ", report)
        if(!is.null(name)) {
            name. <- grepl("author", Rmd) & grepl("\\:", Rmd)
            Rmd[name.] <- paste0("author     : ", strsplit(name, "\\\\")[[1]][1])
        }
        if(slidify == "revealjs") {
            title2. <- Rmd %in% "# title"
            Rmd[title2.] <- paste0("# ", report)
            if(!is.null(name)) {
                name2. <- Rmd %in% "### name"
                Rmd[name2.] <- paste0("### ", strsplit(name, "\\\\")[[1]][1])
            }
            css <- system.file("extdata/docs/style.css", package = "reports")
            file.copy(css,  file.path(y[[4]], "assets/css"))            
        }
        cat(paste(Rmd, collapse="\n"), file = file.path(y[[4]], paste0(report, ".Rmd")))
        delete(file.path(y[[4]], "index.Rmd"))
    }
    AN <- system.file("extdata/docs", package = "reports")
    if (AN.xlsx) {
        invisible(file.copy(file.path(AN, "notes.xlsx"), y[[2]]))   
        invisible(file.rename(file.path(y[[2]], "notes.xlsx"), 
            file.path(y[[2]], paste0("notes_", report, ".xlsx"))))          
    } else {
        invisible(file.copy(file.path(AN, "notes.csv"), y[[2]])) 
        invisible(file.rename(file.path(y[[2]], "notes.csv"), 
            file.path(y[[2]], paste0("notes_", report, ".csv")))) 
    }
    cat("http://", file = file.path(y[[2]], "websites.txt"))
    cat(file = file.path(x, "TO_DO"))
    cat(file = file.path(x, "NOTES"))
    EXF <- "#Source the following project functions on startup"
    cat(EXF, file = file.path(x, "extra_functions.R"))
    invisible(file.copy(pdfloc, y[[1]]))   
    outline <- dat[dat[, "inf"] %in% outline.type, "outf"]
    A <- B <- C <- NULL
    if (any(outline %in%  1)) {
        A <- "outline.docx"
    }
    if (any(outline %in%  2)) {
        B <- c("preamble.tex", "outline.tex")
    }
    if (any(outline %in%  3)) {
        C <- "outline.Rmd"
    }
    fls <- c(A, B, C)
    invisible(file.copy(file.path(root2, fls) , y[[3]]))
    pdfloc3 <- file.path(root2, c("temp.Rmd", "temp.Rnw", "temp.pptx"))
    dir.create(file.path(y[[4]], "figure"), FALSE)
    present.type <- Trim(unlist(strsplit(gsub(desc.chunk2, "", desc[grepl(desc.chunk2, desc)]), ",")))
    matches <- data.frame(grab = pdfloc3,
        required = tolower(tools::file_ext(basename(pdfloc3))), stringsAsFactors = FALSE)
    present.copies <- matches[-1, ][matches[, "required"][-1] %in% present.type , "grab"]
    invisible(file.copy(present.copies, y[[4]])) #copy presentation files to folder
    old.names <- file.path(y[[4]], dir(y[[4]]))
    new.names <- file.path(y[[4]], gsub("temp.", paste0(report, "."), dir(y[[4]]), fixed = TRUE))
    file.rename(old.names, new.names)
    pdfloc4 <- file.path(root2, "TEMP.txt")
    invisible(file.copy(pdfloc4, x))
    invisible(file.rename(file.path(x, "TEMP.txt"), 
        file.path(x, paste0(report, ".Rproj"))))
    pdfloc5 <- file.path(root2, "REPORT_WORKFLOW_GUIDE.pdf")
    invisible(file.copy(pdfloc5, x))
    rpro <- c("#Load the packages used",
        "library(reports); library(slidify); library(slidifyLibraries); library(knitr); library(knitcitations)", 
        "# library(pander)", "")  
    rpro2 <- c("", "#Source \"extra_functions.R\":",
        "source(file.path(getwd(), \"extra_functions.R\"))")
    rpro3 <- sources
    if (!is.null(rpro3)){
        rpro3 <- c("", "#Source these location(s):", 
            paste0("source(\"", rpro3, "\")"))
    }
    bib <- NULL
    if(!is.null(bib.loc) && file.exists(bib.loc)){
        invisible(file.copy(bib.loc, y[[1]]))
        invisible(file.copy(bib.loc, y[[4]]))
        bib <- dir(y[[1]])[tools::file_ext(dir(y[[1]])) == "bib"]
        if (!is.null(getOption("bib.loc"))) {
            bibL <- paste0("options(bib.loc = \"", bib.loc, "\")")
            rpro <- c(rpro, bibL)
        }
    }
    if (!is.null(!is.null(github.user) && file.exists(github.user))) {
        git <- paste0("options(github.user = \"", github.user, "\")")
        rpro <- c(rpro, git)
    }
    cat(paste(c(rpro, rpro2, rpro3), collapse = "\n"), 
        file = file.path(x, ".Rprofile"))
    if (!is.null(bib.loc) && !file.exists(bib.loc)) {
        warning("bib.loc does not exist")
    }
    if (!is.null(bib)) {
        dr2 <- dir(y[[4]])
        drin2 <- dr2[tools::file_ext(dr2) %in% "Rmd"]
        temp2 <- suppressWarnings(readLines(file.path(y[[4]], drin2)))
        temp2 <- gsub("read.bibtex(.bib)", paste0("read.bibtex(\"", bib, "\")"), 
        temp2, fixed = TRUE)
        cat(paste(temp2, collapse="\n"), file=file.path(y[[4]], drin2))
    } 
    if (type %in% c("rnw", "tex")) {
        fp <- file.path(root, template)
        invisible(file.copy(file.path(fp, dir(fp)), paste0(y[[1]], "/")))
        dr <- dir(y[[1]])
        drin <- dr[substring(dr, 1, nchar(dr) -4) %in% "doc"]
        temp <- suppressWarnings(readLines(file.path(y[[1]], drin)))
        if (!is.null(bib)) {
            temp <- gsub("\\.bib", bib, temp)
        }
        temp <- gsub("title\\{.+?\\}", paste0("title{", gsub("_", " ", report),
            "}") , temp)
        if (!is.null(name)) {
            fxbs <- gsub("\\", "\\\\", paste0("author{", name,"}"), fixed = TRUE)
            temp <- gsub("author\\{.+?\\}", fxbs , temp)
        }
        cat(paste(temp, collapse="\n"), file=file.path(y[[1]], drin))
        invisible(file.rename(file.path(y[[1]], drin), 
            file.path(y[[1]], paste0(report, ".", 
            tools::file_ext(drin)))))       
        dr2 <- dir(y[[4]])
        drin3 <- dr2[tools::file_ext(dr2) %in% "Rnw"]
        temp3 <- suppressWarnings(readLines(file.path(y[[4]], drin3)))
        if (!is.null(name)) {
            temp3 <- gsub("author{Author}", paste0("author{", name, "}"), 
                temp3, fixed = TRUE)
        }
        if (!is.null(bib)) {
            temp3 <- gsub("addbibresource{.bib}", paste0("addbibresource{", bib, 
                "}"), temp3, fixed = TRUE) 
        }
        cat(paste(temp3, collapse="\n"), file=file.path(y[[4]], drin3))
        folder(folder.name=file.path(y[[1]], "figure"))
    } else {
        if (type == "doc") {
            fp <- file.path(root, template)
            invisible(file.copy(file.path(fp, dir(fp)), paste0(y[[1]], "/")))     
        } else {
            fp <- file.path(root, template)
            invisible(file.copy(file.path(fp, dir(fp)), paste0(y[[1]], "/")))
            dr <- dir(y[[1]])
            drin <- dr[tools::file_path_sans_ext(dr) %in% "doc"]
            temp <- suppressWarnings(readLines(file.path(y[[1]], drin)))
            temp <- gsub("Title", report, temp)
            if (!is.null(bib)) {
                new.bib <- paste(c("fls <- dir(getwd())", 
                    "BIB <- file.path(getwd(), fls[tools::file_ext(fls) == \"bib\"])"), 
                    collapse="\n")
                temp[grepl("BIB <- system.file", temp)] <- new.bib
            }
            temp <- gsub("Date(?!\\()", paste("Date:", Sys.Date()), temp, perl=TRUE)
            if (!is.null(name)) {
                temp <- gsub("Name", name, temp)
            }
            cat(paste(temp, collapse="\n"), file=file.path(y[[1]], drin))
            invisible(file.rename(file.path(y[[1]], drin), 
                file.path(y[[1]], paste0(report, ".", 
                tools::file_ext(drin))))) 
        }   
    }
    ins <- file.path(pdfloc, "inst")
    if (file.exists(ins)) {
        ins2 <- file.path(ins, dir(ins))
        invisible(lapply(ins2, function(zz) {
           file.copy(zz, x, overwrite = TRUE, recursive = TRUE) 
        }))
        delete(file.path(x, "slidify.Rmd"))
    }  
    o <- paste0("Report \"", report, "\" created:\n", x, "\n")
    class(o) <- "reports"
    return(o)    
}


#' Prints a reports Object
#' 
#' Prints a reports object.
#' 
#' @param x The reports object.
#' @param \ldots ignored
#' @method print reports
#' @S3method print reports
print.reports <-
function(x, ...) {
    class(x) <- NULL
    cat(x)
}

