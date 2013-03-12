\name{CA}
\alias{CA}
\title{Capitalize Every Word of String}
\usage{
  CA(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector every word capitalized.
}
\description{
  Capitalizes every word of a string.
}
\details{
  This capitalizes every word of a string.
}
\section{Warning}{
  Ligatures are assumed to be "fi", however, these elements
  may be "ff", "fi", "fl", "ffi" or "ffl".
}
\examples{
CA("the flexible, efficient way to do reports.")
}

