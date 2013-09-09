\name{QP}
\alias{QP}
\title{Quick Path}
\usage{
  QP(...)
}
\arguments{
  \item{\ldots}{The directory (or nested directories)
  within the working directry.}
}
\value{
  Returns a path.
}
\description{
  This function is a wrapper for
  \code{\link[base]{file.path}} and
  \code{\link[base]{getwd}}
}
\examples{
QP()
QP("PRESENTATION")
QP("PRESENTATION/figure")
QP("PRESENTATION", "figure")
}
\seealso{
  \code{\link[base]{file.path}}, \code{\link[base]{getwd}}
}

