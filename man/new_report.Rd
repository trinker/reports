\name{new_report}
\alias{new_report}
\title{Report Template}
\usage{
  new_report(report = "report",
    template = "apa6.mod.qual_tex",
    bib.loc = getOption("bib.loc"),
    name = getOption("name_reports"), path = getwd(),
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

  \item{name}{A character vector of the user's name to be
  used on the report.}

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
\note{
  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc} and \code{name_reports} in the .Rprofile.
}
\section{Additional Guide}{
  Introductory video
  \url{http://www.youtube.com/watch?v=qBgsJG546gE&feature=youtu.be}
}
\examples{
\dontrun{
new_report()
}
}
\seealso{
  \code{\link[reports]{doc_temp}}
}

