\name{RN}
\alias{RN}
\title{Insert reveal.js Notes}
\usage{
  RN(text = "clipboard", copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If FALSE returns to
  the console.}
}
\value{
  Returns a character vector wrapped with a courier new
  font tag.
}
\description{
  Creates an aside of the class "notes" for reveal.js
  slides
}
\examples{
RN("some fancy notes", print = TRUE)
RN("1) some\\n2) fancy\\n3) notes")
RN("1) some
2) fancy
3) notes", print = TRUE)
}

