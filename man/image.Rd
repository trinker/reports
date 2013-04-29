\name{IM}
\alias{IM}
\alias{IM2}
\title{Convert path/url to HTML Image Tag}
\usage{
  IM(path = "clipboard", width = 540,
    height = IE(width, 360),
    sty = IE(width, width * 1.05, 480), center = TRUE,
    link = NULL, copy2clip = TRUE, print = FALSE)

  IM2(image = "clipboard", ...)
}
\arguments{
  \item{path}{character vector url/path to the image.
  Default is to read from the clipboard.  Note that Windows
  users do not have to reorient slashes in local paths if
  reading from the clipboard.}

  \item{width}{the width of the image.  If \code{NULL} the
  defualt image width is used.}

  \item{height}{the height of the image.  If \code{NULL}
  the defualt image height is used.}

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

  \item{image}{character vector name of the image. Default
  is to read from the clipboard.}

  \item{\ldots}{other arguments passed to \code{IM}.}
}
\value{
  Returns a character vector of an HTML image tag that
  embeds an image.
}
\description{
  \code{IM} - Wrap a path/url to generate an HTML tag.
  Often markup code: \code{![](url)} lacks flexibility with
  centering and sizing.  \code{IM} enables conrol of
  centering via altering the sty/center commands and
  control of sizing via the numeric values supplied to
  height and width.

  \code{IM2} - A wrapper for \code{IM} that sets the base
  path to "assets/img/". This allows the users to just
  specify the image name that resides in the directory:
  "assets/img/".
}
\examples{
IM("http://cran.r-project.org/Rlogo.jpg", width= NULL, print=TRUE)
IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print =TRUE)
IM("http://cran.r-project.org/Rlogo.jpg", NULL, print=TRUE, link = "http://cran.r-project.org")
}

