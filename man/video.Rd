\name{YT}
\alias{VM}
\alias{YT}
\title{Video (YouTube/Vimeo) url to HTML iframe Tag}
\usage{
  YT(path = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)

  VM(path = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)
}
\arguments{
  \item{path}{A character vector url/tag copied to the
  clipboard. Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{width}{The width of the player.}

  \item{height}{The height of the player.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}
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
\note{
  For YouTube videos it may be better to utilize a slide
  layout class as a slide with a link followed by a slide
  with a youtube video may cause the link to become in
  active.  To create a youtube slide use the form:
  \code{--- .YT yt:ArHQjQyIS70 &youtube} where the portion
  after \code{yt:} is the youtube tag.
}
\examples{
YT("ArHQjQyIS70", print = TRUE)
YT("http://www.youtube.com/watch?v=ArHQjQyIS70", print = TRUE)
VM("http://vimeo.com/54007714", print = TRUE)
}
\seealso{
  \code{\link[reports]{slidify_layouts}}
}

