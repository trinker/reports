\name{presentation}
\alias{presentation}
\title{Report Template}
\usage{
  presentation(presentation = "presentation", rnw = TRUE,
    theme = "Madrid", bib.loc = getOption("bib.loc"),
    path = getwd(), name = getOption("name_reports"))
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

  The user may want to set \code{\link[base]{options}} for
  \code{bib.loc} and \code{name_reports} in the .Rprofile.
}
\examples{
\dontrun{
presentation()
presentation("New", rnw=TRUE, theme=NULL)
presentation("New", rnw=FALSE)
}
}
\seealso{
  \code{link[reports]{new_report}}
}

