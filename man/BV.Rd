\name{BV}
\alias{BV}
\title{View Bibliography}
\usage{
  BV(bib = NULL, col.width = 40)
}
\arguments{
  \item{bib}{Path to bibliography.  If \code{NULL}
  \code{BV} attempts to locate the .bib file (working
  directory first and then "main_dir/REPORTS/_.bib", last
  "main_dir/PRESENTATION/_.bib" in that order).}

  \item{col.width}{An integer value of the maximum width of
  columns.}
}
\value{
  Returns a truncated view of user notes.
}
\description{
  Allows a truncated view of your .bib.
}
\examples{
\dontrun{
notes()
}
}

