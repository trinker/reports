\name{IM}
\alias{IM}
\title{Convert path/url to HTML Image Tag}
\usage{
  IM(text = "clipboard", width = 400, height = 300,
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{text}{character vector url/path copied to the
  clipboard. Default is to read from the clipboard.  Note
  that Windows users do not have to reorient slashes in
  local paths if reading from the clipboard.}

  \item{width}{the width of the image.}

  \item{height}{the height of the image.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code[base]{cat} prints
  the output to the console.  If FALSE returns to the
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

