\name{new_report}
\alias{new_report}
\title{Report Template}
\usage{
  new_report(report = "report",
    template = "apa6.mod.qual_tex",
    bib.loc = getOption("bib.loc"),
    name = getOption("name_reports"),
    sources = getOption("sources_reports"), path = getwd(),
    AN.xlsx = TRUE)
}
\arguments{
  \item{report}{A character vector of the project name.}

  \item{template}{A character vector of the internal
  reports template or an external path to a template in the
  reports package style.}

  \item{bib.loc}{Optional path to a .bib resource.}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}

  \item{name}{A character string of the user's name to be
  used on the report.}

  \item{sources}{A vector of path(s) to other scripts to be
  sourced in the report project upon startup (adds this
  location to the report project's \code{.Rprofile}).}

  \item{AN.xlsx}{logical.  If \code{TRUE} the article notes
  (AN) will be in .xlsx format.  If \code{FALSE} the
  document will be a .csv file..}
}
\value{
  Creates a report template.
}
\description{
  Generate a report/paper template to increase efficiency.
}
\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc}, \code{name_reports} and
  \code{sources_reports} in the users primary
  \code{.Rprofile}.
}

\section{Additional Guide}{
  Introductory video \url{http://youtu.be/cokNUTGtoM4}
}
\examples{
\dontrun{
new_report()
}
}
\seealso{
  \code{\link[reports]{doc_temp}},
  \code{\link[reports]{presentation}}
}

