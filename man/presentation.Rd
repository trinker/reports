\name{presentation}
\alias{presentation}
\title{Presentation Template}
\usage{
  presentation(presentation = "presentation", rnw = TRUE,
    theme = "Madrid", bib.loc = getOption("bib.loc"),
    name = getOption("name_reports"),
    sources = getOption("sources_reports"), path = getwd())
}
\arguments{
  \item{presentation}{A character vector of the project
  name.}

  \item{rnw}{logical.  If \code{TRUE} the docuemnts will be
  .Rnw and .tex files.  If \code{FALSE} the documents will
  be .pptx and .docx files.}

  \item{theme}{\href{http://deic.uab.es/~iblanes/beamer_gallery/index_by_theme.html}{Beamer
  theme} to use.  If \code{NULL} \code{presentation} will
  allow the user to choose interactively.}

  \item{bib.loc}{Optional path to a .bib resource.}

  \item{name}{A character vector of the user's name to be
  used on the report.}

  \item{sources}{A vector of path(s) to other scripts to be
  sourced in the report project upon startup (adds this
  location to the report project's \code{.Rprofile}).}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}
}
\value{
  Creates a report template.
}
\description{
  Generate a presentation template to increase efficiency.
  This isa lighter weight version of
  \code{\link[reports]{new_report}}.
}
\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc}, \code{name_reports} and
  \code{sources_reports} in the user's primary
  \code{.Rprofile}.
}
\examples{
\dontrun{
presentation()
presentation("New", rnw=TRUE, theme=NULL)
presentation("New", rnw=FALSE)
}
}
\seealso{
  \code{\link[reports]{new_report}}
}

