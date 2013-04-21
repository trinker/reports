\name{HR}
\alias{HR}
\title{Convert path/url to HTML href Tag}
\usage{
  HR(text = NULL, path = "clipboard", copy2clip = TRUE,
    print = TRUE)
}
\arguments{
  \item{text}{character vector of text to hyperref from.
  Defualt uses the \code{link[base]{basename}} of the
  path.}

  \item{path}{character vector url/path copied to the
  clipboard. Default is to read from the clipboard.  Note
  that Windows users do not have to reorient slashes in
  local paths.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE prints the output to the
  console.}
}
\value{
  Returns a character vector of an HTML href tag.
}
\description{
  Wrap a path/url to generate an HTML href tag.
}
\examples{
## HR("new", "assets/img/fry_admin_1.mp4")
}

