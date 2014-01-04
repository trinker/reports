\name{PN}
\alias{PN}
\title{Insert Presenter Notes}
\usage{
PN(text = "clipboard", copy2clip = interactive(), print = FALSE)
}
\arguments{
  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}
}
\value{
Returns a character vector wrapped with HTML aside tags.
}
\description{
Creates an aside of the class "notes" for slides (e.g.,
reveal.js slides).
}
\examples{
PN("some fancy notes", print = TRUE)
PN("1) some\\n2) fancy\\n3) notes", print = TRUE)
PN("1) some
2) fancy
3) notes", print = TRUE)
}

