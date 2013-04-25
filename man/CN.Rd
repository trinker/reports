\name{CN}
\alias{CN}
\title{Wrap Text With HTML Courier New Font Tag}
\usage{
  CN(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector wrapped with a courier new
  font tag.
}
\description{
  Wraps text with a courier new font tag. A specified
  version of \code{QF} but more convient for constant use
  of courier new tags.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}
\examples{
CN("new_report()")
}

