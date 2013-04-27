\name{IM}
\alias{IM}
\title{Convert path/url to HTML Image Tag}
\usage{
  IM(path = "clipboard", width = NULL, height = NULL,
    sty = NULL, center = TRUE, link = NULL,
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{path}{character vector url/path to the image.
  Default is to read from the clipboard.  Note that Windows
  users do not have to reorient slashes in local paths if
  reading from the clipboard.}

  \item{width}{the width of the image.}

  \item{height}{the height of the image.}

  \item{sty}{the width of the style (used for centering).}

  \item{center}{logical.  If TRUE the image will be
  centered, if FALSE image will be left justified.}

  \item{link}{character vector url/path to hyperlink the
  image to.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If FALSE returns to
  the console.}
}
\value{
  Returns a character vector of an HTML image tag that
  embeds an image.
}
\description{
  Wrap a path/url to generate an HTML tag.  Often markup
  code: \code{![](url)} lacks flexibility with centering
  and sizing.  \code{IM} enables conrol of centering via
  altering the sty/center commands and control of sizing
  via the numeric values supplied to height and width.
}
\examples{
IM("http://cran.r-project.org/Rlogo.jpg", print=TRUE)
IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print =TRUE)
IM("http://cran.r-project.org/Rlogo.jpg", print=TRUE, link = "http://cran.r-project.org")
}

