\name{pad}
\alias{pad}
\title{Pad Strings}
\usage{
  pad(x, padding = max(nchar(as.character(x))),
    sort = TRUE, type = "detect")
}
\arguments{
  \item{x}{A character, factor, numeric vector.}

  \item{padding}{Number of characters to pad.  Default
  makes all elements of a string the number of characters
  of the element with the maximun characters.}

  \item{sort}{logical.  If \code{TRUE} the outcome is
  sorted.}

  \item{type}{A character string of \code{"detect"},
  \code{"numeric"}, \code{"character"}, \code{"d"} or
  \code{"s"}.  If numeric zeros are padded. If character
  spaces are padded.  The \code{detect} attempts to
  determine if x is numeric (d) or not (s).}
}
\value{
  Returns a charcter vector every element padded with
  0/spaces.
}
\description{
  A convenience wrapper for \code{\link[base]{sprintf}}
  that pads the front end of strings with spaces or 0s.
  Useful for creating multiple unifor directories that will
  maintain correct order.
}
\examples{
pad(sample(1:10, 6))
pad(sample(1:10, 6), sort=FALSE)
pad(as.character(sample(1:10, 6)))
pad(as.character(sample(1:10, 6)), 4)
}
\seealso{
  \code{\link[base]{sprintf}}
}

