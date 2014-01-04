\name{CM}
\alias{CM}
\title{HTML Comment Character}
\usage{
CM(comment = "clipboard", copy2clip = interactive(), print = FALSE)
}
\arguments{
  \item{comment}{A text comment.  Default copies from the
  clipboard.}

  \item{copy2clip}{If \code{TRUE} attempts to copy the
  output to the clipboard.}

  \item{print}{logical. If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} \code{\link[base]{return}}s to the
  console.}
}
\value{
Returns a character vector of an HTML comment tag.
}
\description{
Converts text to an HTML comment tag
}
\examples{
\dontrun{
CM("strike me from the record")
}
}
\references{
\url{http://www.w3schools.com/tags/tag_comment.asp}
}
\keyword{comment}

