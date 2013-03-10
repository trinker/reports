\name{templates}
\alias{templates}
\title{View Templates}
\usage{
  templates(print.cons = TRUE, names = TRUE)
}
\arguments{
  \item{print.cons}{logical.  If \code{TRUE} The template
  information id printed to the console.}

  \item{names}{logical.  If \code{TRUE} only the names of
  the templates will be returned.}
}
\description{
  View all the reports package templates.
}
\details{
  This function prints (via \code{\link[base]{cat}}) to the
  console to view possible templates (feel free to submit
  your own; see \code{\link[reports]{doc_temp}} for
  details) as well as optionally returning a vector of
  names that the user can pass to other functions.
}
\examples{
\dontrun{
templates()
templates(FALSE)
new_report("new", templates(FALSE)[4])
}
}
\seealso{
  \code{\link[reports]{doc_temp}}
}

