\name{presentation}
\alias{presentation}
\title{Presentation Template}
\usage{
  presentation(presentation = "presentation",
    type = c("rnw", "rmd"), theme = "Madrid",
    bib.loc = getOption("bib.loc"),
    name = getOption("name.reports"),
    github.user = getOption("github.user"),
    sources = getOption("sources.reports"), path = getwd(),
    slidify = getOption("slidify.template"),
    open = is.global(), ...)
}
\arguments{
  \item{presentation}{A character vector of length two or
  one: (1) the main directory name and (2) sub directory
  names (i.e., all the file contents will be imprinted with
  this name). If the length of \code{report} is one this
  name will be used as the main directory name and all sub
  directories and files.}

  \item{type}{A vector of the file format types.  Any
  combination of the following: \code{rnw}, \code{rmd} or
  \code{pptx}.  \code{rnw} corresponds to a beamer slides
  (.Rnw file), \code{rmd} corresponds to a html5
  (compliments of slidify) slides (.Rnwd file) and
  \code{docx} corresponds to PowerPoint slides (.pptx
  file).}

  \item{theme}{\href{http://deic.uab.es/~iblanes/beamer_gallery/index_by_theme.html}{Beamer
  theme} to use.  If \code{NULL} \code{presentation} will
  allow the user to choose interactively.}

  \item{bib.loc}{Optional path to a .bib resource.}

  \item{name}{A character vector of the user's name to be
  used on the report.}

  \item{github.user}{GitHub user name (character string).}

  \item{sources}{A vector of path(s) to other scripts to be
  sourced in the report project upon startup (adds this
  location to the report project's \code{.Rprofile}).}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}

  \item{slidify}{The template to be used in the
  PRESENTATION .Rmd.  This can be one of the types from
  \code{slidify_templates} or a path to an .Rmd file. This
  argument will be overrode if a custom reports template is
  supplied with an .Rmd file in the inst directory named
  slidify.Rmd (\code{/inst/slidify.Rmd}).}

  \item{open}{logical.  If \code{TRUE} the project will be
  opened in RStudio. The default is to test if
  \code{new_report} is being used in the global
  environment, if it is then the project directory will be
  opened.}

  \item{\ldots}{Other arguments passed to
  \code{\link[slidify]{author}}.}
}
\value{
  Creates a presentation template.
}
\description{
  Generate a presentation template to increase efficiency.
  This is a lighter weight version of
  \code{\link[reports]{new_report}} that focuses on the
  presentation.
}
\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc}, \code{github.user}, \code{name.reports}
  \code{sources.reports},\code{slidify.template} and
  \code{reveraljs.loc} in the user's primary
  \code{.Rprofile}: \enumerate{ \item{\bold{bib.loc} - The
  path to the user's primary bibliography}
  \item{\bold{github.user} - GitHub user name}
  \item{\bold{name.reports} - The name to use on reports}
  \item{\bold{sources.reports} - Path(s) to additional
  files/scripts that should be included to be sourced in
  the project startup} \item{\bold{slidify.template} - Path
  to, or defualt, .Rmd file tempalte for use in as the .Rmd
  used in the slidify presentations (see
  \code{slidify_templates} for possible non-path
  arguments)} }
}
\examples{
## presentation("New")
}
\references{
  \href{https://github.com/ramnathv/slidifyExamples/tree/gh-pages/examples}{slidify
  examples}
}
\seealso{
  \code{\link[reports]{new_report}},
  \code{\link[reports]{slidify_templates}},
  \code{\link[slidify]{author}}

  \href{https://github.com/hakimel/reveal.js/}{Installation
  section of reveal.js GitHub}
}

