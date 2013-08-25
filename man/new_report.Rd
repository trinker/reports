\name{new_report}
\alias{new_report}
\title{Report Template}
\usage{
  new_report(report = "report",
    template = getOption("temp.reports"),
    bib.loc = getOption("bib.loc"),
    name = getOption("name.reports"),
    github.user = getOption("github.user"),
    sources = getOption("sources.reports"), path = getwd(),
    AN.xlsx = TRUE,
    slidify = getOption("slidify.template"), open = FALSE,
    ...)
}
\arguments{
  \item{report}{A character vector of length two or one:
  (1) the main directory name and (2) sub directory names
  (i.e., all the file contents will be imprinted with this
  name). If the length of \code{report} is one this name
  will be used as the main directory name and all sub
  directories and files.}

  \item{template}{A character string of the internal
  reports template or an external path to a template in the
  reports package style.  This argument allows the user to
  change the contents of the report directory that is
  generated. See \code{templates} for more.}

  \item{bib.loc}{Optional path to a .bib resource.}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}

  \item{name}{A character string of the user's name to be
  used on the report.}

  \item{github.user}{GitHub user name (character string).}

  \item{sources}{A vector of path(s) to other scripts to be
  sourced in the report project upon startup (adds this
  location to the report project's \code{.Rprofile}).}

  \item{AN.xlsx}{logical.  If \code{TRUE} the article notes
  (AN) will be in .xlsx format.  If \code{FALSE} the
  document will be a .csv file.}

  \item{slidify}{The template to be used in the
  PRESENTATION .Rmd.  This can be one of the types from
  \code{slidify_templates} or a path to an .Rmd file. This
  argument will be overrode if a custom reports template is
  supplied with an .Rmd file in the inst directory named
  slidify.Rmd (\code{/inst/slidify.Rmd}).}

  \item{open}{logical.  If \code{TRUE} the project will be
  opened in RStudio.}

  \item{\ldots}{Other arguments passed to
  \code{\link[slidify]{author}}.}
}
\value{
  Creates a report template.
}
\description{
  Generate a report/paper template to increase efficiency.
}
\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc}, \code{github.user}, \code{name.reports}
  \code{sources.reports},\code{slidify.template} and
  \code{reveraljs.loc} in the user's primary
  \code{.Rprofile}: \enumerate{ \item{\bold{bib.loc} - The
  path to the users primary bibliography}
  \item{\bold{name.reports} - The name to use on reports}
  \item{\bold{temp.reports} - The primary template to use
  to generate reports (see \code{template})}
  \item{\bold{github.user} - GitHub user name}
  \item{\bold{speed.temp} - A speed dial like interface
  that allows the template argument to take a numeric
  arguement.  Setting this option takes the form of:\cr
  \code{options(speed.temp=list(`1`="wordpress_rmd",
  `2`="basic_rmd"))}} \item{\bold{sources.reports} -
  Path(s) to additional files/scripts that should be
  included to be sourced in the project startup}
  \item{\bold{slidify.template} - Path to, or defualt, .Rmd
  file tempalte for use in as the .Rmd used in the slidify
  presentations (see \code{slidify_templates} for possible
  non-path arguments)} }
}

\section{Additional Guide}{
  Introductory video
  \url{http://www.youtube.com/watch?v=ArHQjQyIS70}
}
\examples{
## new_report()
}
\references{
  \href{https://github.com/ramnathv/slidifyExamples/tree/gh-pages/examples}{slidify
  examples}
}
\seealso{
  \code{\link[reports]{doc_temp}},
  \code{\link[reports]{presentation}},
  \code{\link[reports]{templates}},
  \code{\link[reports]{slidify_templates}},
  \code{\link[slidify]{author}}

  \href{https://github.com/hakimel/reveal.js/}{Installation
  section of reveal.js GitHub}
}

