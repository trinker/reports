\name{UF}
\alias{PF}
\alias{UF}
\title{Path/URL: Fill Blank Spaces}
\usage{
  UF(path = "clipboard", copy2clip = TRUE, fill = "_")

  PF(path = "clipboard", copy2clip = TRUE, fill = "\%")
}
\arguments{
  \item{path}{A character vector or url/path copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{fill}{The character symbol to fill the blank spaces
  with.}
}
\value{
  Returns a character vector every space replaced with an
  underscore/percent And apostophes removed.  \code{UF}
  also converts the text to lower case.
}
\description{
  \code{UF} - A convenience function to replace black
  spaces of paths with underscores.

  \code{PF} - A convenience function to replace black
  spaces of URLs with percents.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}
\examples{
UF("bad path with spaces")
PF("https://github.com/trinker/reports/fictional path to nowhere.pdf")
}

