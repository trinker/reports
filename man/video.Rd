\name{YT}
\alias{VM}
\alias{YT}
\title{Video (YouTube/Vimeo) url to iframe HTML Tag}
\usage{
  YT(text = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)

  VM(text = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)
}
\arguments{
  \item{text}{character vector url/tag copied to the
  clipboard. Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{width}{the width of the player.}

  \item{height}{the height of the player.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If FALSE returns to
  the console.}
}
\value{
  Returns a character vector of an HTML iframe tag that
  embeds a YouTube or Vimeo video.
}
\description{
  Wrap a YouTube/Vimeo tag or url to generate an HTML
  iframe tag.
}
\details{
  Use \code{YT} for YouTube videos and \code{VM} for Vimeo
  videos.
}
\examples{
YT("kws1PX1Dw9w", print = TRUE)
YT("http://www.youtube.com/watch?v=kws1PX1Dw9w", print = TRUE)
VM("http://vimeo.com/54007714", print = TRUE)
}

