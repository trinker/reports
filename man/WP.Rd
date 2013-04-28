\name{WP}
\alias{WP}
\title{Convert Windows Path}
\usage{
  WP(text = "clipboard", quotes = TRUE, copy2clip = TRUE)
}
\arguments{
  \item{clipboard}{logical.  If TRUE copies the path from
  the clipboard, if FALSE \code{WP} will ask for the path
  interactively.}

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

