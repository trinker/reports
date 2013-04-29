\name{WP}
\alias{WP}
\title{Convert Windows Path}
\usage{
  WP(path = "clipboard", quotes = TRUE, copy2clip = TRUE)
}
\arguments{
  \item{path}{character vector url/path copied to the
  clipboard. Default is to read from the clipboard.}

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

