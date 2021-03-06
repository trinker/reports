% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/IM.R
\name{IM}
\alias{IM}
\alias{IM2}
\alias{IMF}
\alias{IM_MO}
\alias{IW}
\title{Convert path/url to HTML Image Tag}
\usage{
IM(path = "clipboard", link = NULL, width = 540, height = IE(width,
  round(width/1.5)), sty = IE(width, width * 1.05, 480), center = TRUE,
  new_win = TRUE, copy2clip = interactive(), print = FALSE)

IM2(image = "clipboard", loc = 1, ...)

IW(path = "clipboard", link = NULL, side = "right", width = 540,
  height = IE(width, round(width/1.5)), new_win = TRUE, top = -15,
  right = 20, left = 20, bottom = 0, copy2clip = interactive(),
  print = FALSE)

IMF(image = "clipboard", ...)

IM_MO(path = "clipboard", link = NULL, width = 32, height = 32,
  width2 = width * 10, height2 = height * 10, sty = width * 1.05,
  center = FALSE, new_win = TRUE, copy2clip = interactive(),
  print = FALSE)
}
\arguments{
\item{path}{A character vector url/path to the image. Default is to
read from the clipboard.  Note that Windows users do not have to reorient
slashes in local paths if reading from the clipboard.}

\item{link}{character vector url/path to hyperlink the image to.}

\item{width}{The width of the image.  If \code{NULL} the defualt image width
is used.}

\item{height}{The height of the image.  If \code{NULL} the defualt image
height is used.}

\item{sty}{The width of the style (used for centering).}

\item{center}{logical. If \code{TRUE} the image will be centered, if
\code{FALSE} image will be left justified.}

\item{new_win}{logical.  If \code{TRUE} the link will open in a new window.}

\item{copy2clip}{logical.  If \code{TRUE} attempts to copy the output to the
clipboard.}

\item{print}{logical.  If \code{TRUE} \code{\link[base]{cat}} prints the output to the
console.  If FALSE returns to the console.}

\item{image}{A character vector name of the image. Default is to read from the
clipboard.}

\item{loc}{The location of the image: 1-/assets/img and 2-/figure.}

\item{side}{The side the image should appear on c(\code{"left"}, \code{"right"}).}

\item{top}{Space between top margin and top of text.}

\item{right}{Space on the right margin.}

\item{left}{Space on the left margin..}

\item{bottom}{Space between bottom margin and bottom of text (0 is defualt).}

\item{width2}{The width to grow to during mouseover.}

\item{height2}{The height to grow to during mouseover.}

\item{\ldots}{Other arguments passed to \code{IM}.}
}
\value{
Returns a character vector of an HTML image tag that embeds an image.
}
\description{
\code{IM} - Wrap a path/url to generate an HTML tag.  Often markup code:
\code{![](url)} lacks flexibility with centering and sizing.  \code{IM}
enables conrol of centering via altering the sty/center commands and control
of sizing via the numeric values supplied to height and width.

\code{IM2} - A wrapper for \code{IM} that sets the base path to "assets/img/".
This allows the users to just specify the image name that resides in one of
the following directories: 1-"\code{~/assets/img}" or 2-"\code{~/figure}".

\code{IW} - Text wrapped images.

\code{IMF} - A wrapper for \code{IM} that attempts to find the image in expected
image directories.  Uses the first available image matching the image name.  No
path/url is specified.  IMS stands for `image locate`.

\code{IM_MO} - Creates an HTML image tag with mouseover grow/shrink
properties.  Only takes integer width/height values converted to px.
}
\note{
\code{IM_MO} Requires a link to the \file{js/reports.js} in the
document as well as the document itself.  Usually this is done automatically
upon the use of \code{new_report}/\code{presentation}.  \code{IM_MO} only
takes integer width/height values which are converted to px.
}
\examples{
IM("http://cran.r-project.org/Rlogo.jpg", width= NULL, print=TRUE)
IM("https://dl.dropboxusercontent.com/u/61803503/packages/reports.PNG", print =TRUE)
IM("http://cran.r-project.org/Rlogo.jpg", NULL, print=TRUE, link = "http://cran.r-project.org")
cat(IW("http://www.talkstats.com/images/misc/logo.png", "http://www.talkstats.com/",
    width=140, height=75), rep("So much text! ", 100))
IM_MO("http://i.imgur.com/VClk4DS.png", print =TRUE)
}

