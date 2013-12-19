\name{WP}
\alias{WP}
\title{Convert Windows Path}
\usage{
  WP(from.clip = TRUE, quotes = TRUE,
    copy2clip = interactive())
}
\arguments{
  \item{from.clip}{logical.  If \code{TRUE} path is read
  from the clipboard. If \code{FALSE} user inputs path
  inteactively.}

  \item{quotes}{logical.  If \code{TRUE} quotes will be
  added to front/end of the string.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector path with slashes oriented for
  R.
}
\description{
  Reorients Windows path backslashes to forward slashes.
}
\examples{
## WP(FALSE)  #using readline <br>
## C:\\Users\\trinker\\Desktop\\doc
}

