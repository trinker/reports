\name{US}
\alias{US}
\title{Insert Underscores in Path}
\usage{
  US(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector every space replaced with an
  underscore.
}
\description{
  A convenience wrapper to replace black spaces with
  underscores.
}
\section{Warning}{
  Ligatures are assumed to be "fi", however, these elements
  may be "ff", "fi", "fl", "ffi" or "ffl".
}
\examples{
US("bad path with spaces")
}

