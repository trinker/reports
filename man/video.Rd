\name{YT}
\alias{VM}
\alias{YT}
\title{Video (YouTube/Vimeo) url to iframe HTML Tag}
\usage{
  YT(text = "clipboard", copy2clip = TRUE)

  VM(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector url/tag copied to the
  clipboard. Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
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
## YT("kws1PX1Dw9w")
## YT("http://www.youtube.com/watch?v=kws1PX1Dw9w")
## VM("http://vimeo.com/54007714")
}

