\name{IM}
\alias{IM}
\title{Convert path/url to HTML Tag}
\usage{
  IM(text = "clipboard", copy2clip = TRUE, print = TRUE)
}
\arguments{
  \item{text}{character vector url/path copied to the
  clipboard. Default is to read from the clipboard.  Note
  that Windows users do not have to reorient slashes in
  local paths.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE prints the output to the
  console.}
}
\value{
  Returns a character vector of an HTML image tag that
  embeds an image.
}
\description{
  Wrap a path/url to generate an HTML tag.  Often markup
  code: \code{![](url)} lacks flexibility with centering
  and sizing.  \code{IM} enables conrol of centering via
  altering the numeric value in \code{width:420px} and
  sizing via the numeric values supplied to height and
  width.
}
\examples{
IM("http://cran.r-project.org/Rlogo.jpg")
}

