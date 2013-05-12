\name{IF}
\alias{IF}
\title{url to iframe HTML Tag}
\usage{
  IF(path = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, center = TRUE, print = FALSE,
    text = "Your browser does not support iframes.")
}
\arguments{
  \item{path}{A character vector url/tag copied to the
  clipboard. Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{width}{The width of the player.}

  \item{height}{The height of the player.}

  \item{center}{logical. If TRUE the image will be
  centered, if FALSE image will be left justified.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE}returns to the console.}

  \item{text}{character vector of text to display for
  browser that do not support iframe tags.}
}
\value{
  Returns a character vector of an HTML iframe tag that
  embeds a document.
}
\description{
  Wrap a url to generate an HTML iframe tag.
}
\examples{
IF("https://dl.dropboxusercontent.com/u/61803503/MWE.html", print = TRUE)
IF("http://www.online-stopwatch.com/countdown-clock/full-screen/", height=400, center = TRUE, print = TRUE)
IF("http://awwapp.com/draw.html", width=700, height=500, print=TRUE)
IF("http://glimmer.rstudio.com/pssguy/TVShowRatings/", width="100\%", height=650, print=TRUE)
IF("https://dl.dropboxusercontent.com/u/61803503/Slides/reports/index.html", width=1150, height=750, print=TRUE)
}

