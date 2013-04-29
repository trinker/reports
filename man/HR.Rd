\name{HR}
\alias{HR}
\title{Convert path/url to HTML href Tag}
\usage{
  HR(path = "clipboard", text = NULL, new_win = TRUE,
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{path}{character vector url/path copied to the
  clipboard. Default is to read from the clipboard.  Note
  that Windows users do not have to reorient slashes in
  local paths if reading from the clipboard.}

  \item{text}{character vector of text to hyperref from.
  Defualt uses the \code{\link[base]{basename}} of the
  path.}

  \item{new_win}{logical.  If \code{TRUE} the link will
  open in a new window.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If FALSE returns to
  the console.}
}
\value{
  Returns a character vector of an HTML href tag.
}
\description{
  Wrap a path/url to generate an HTML href tag.
}
\examples{
## HR("assets/img/fry_admin_1.mp4","new")

HR(path="http://dl.dropbox.com/u/61803503/Likert.pdf", print = TRUE)
HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", print = TRUE)
HR("http://cran.r-project.org/src/contrib/reports_0.1.2.tar.gz", "click me", print = TRUE)
}

