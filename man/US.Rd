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
  Returns a character vector every space repalced with an
  underscore.
}
\description{
  A convienence wrapper to replace black spaces with
  underscores.
}
\examples{
US("bad path with spaces")
}

