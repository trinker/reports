\name{UF}
\alias{PF}
\alias{UF}
\alias{US}
\title{Path/URL: Fill Blank Spaces}
\usage{
UF(path = "clipboard", copy2clip = interactive(), fill = "_")

PF(path = "clipboard", copy2clip = interactive(), fill = "\%")

US(path = "clipboard", copy2clip = interactive(), fill = "_")
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
underscore/percent And apostophes removed.  \code{UF} also
converts the text to lower case.
}
\description{
\code{UF} - A convenience function to replace black spaces
of paths with underscores.

\code{PF} - A convenience function to replace black spaces
of URLs with percents.

\code{US} - A convenience function to replace black spaces
of paths with underscores.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  may be incorrect.  If a warning is thrown check the use
  of "ff", "fi", "fl", "ffi" and "ffl".
}

\section{Notice}{
  \code{US} is deprecated and will be removed from reports
  in a version subsequent to version 0.2.0 Please use
  \code{UF} instead.
}
\examples{
UF("bad path with spaces")
PF("https://github.com/trinker/reports/fictional path to nowhere.pdf")
}

