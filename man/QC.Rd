\name{QC}
\alias{QC}
\title{Pandoc Convert a String}
\usage{
  QC(to = "latex", from = "markdown", text = "clipboard",
    copy2clip = TRUE)
}
\arguments{
  \item{to}{Pandoc type you're converting to (defualt is
  \code{"latex"}).}

  \item{to}{Pandoc type you're converting from (defualt is
  \code{"markdown"}).}

  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector every word capitalized.
}
\description{
  Uses Pandoc to convert from one form to another.
}
\details{
  This capitalizes every word of a string.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}
\examples{
x <- "\\\\*note: I *like* chocolate **milk** too ***much***!"
QC(x)
QC(x, "html")
}
\author{
  mnel of stackoverflow.com and Tyler Rinker<tyler.rinker
}
\references{
  \href{http://johnmacfarlane.net/pandoc/demos.html}{Pandoc
  Website}
  \url{http://stackoverflow.com/a/15539563/1000343}
}

