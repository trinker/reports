\name{YT}
\alias{VD}
\alias{VM}
\alias{YT}
\title{Video (YouTube/Vimeo/local) url to HTML iframe/video Tag}
\usage{
  YT(path = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)

  VM(path = "clipboard", copy2clip = TRUE, width = 640,
    height = 360, print = FALSE)

  VD(path = "clipboard",
    video.js.path = "assets/js/vidscript.js",
    width = "100\%", height = 520, indent.controls = 4,
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{path}{A url/tag/path. Default is to read from the
  clipboard.  Note that \code{VD} requires a .mp4 file.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{width}{The width of the player.}

  \item{height}{The height of the player.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}

  \item{video.js.path}{The path to the vidscript.js.  If
  the file is not found in the \code{\link[base]{dirname}}
  of \code{video.js.path} then
  \code{\link[reports]{js_copy}} will be utilized to place
  the appropriate video file in the correct location.}

  \item{indent.controls}{An integer value for number of
  indents to push the control panel.}
}
\value{
  Returns a character vector of an HTML iframe tag that
  embeds a YouTube or Vimeo video.
}
\description{
  \code{YT} - Wrap a YouTube tag or url to generate an HTML
  iframe tag.

  \code{VM} - Wrap a Vimeo tag or url OR to generate an
  HTML iframe tag.

  \code{VD} - Wrap a local path to generate an HTML video
  tag.
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
\references{
  \url{http://blog.teamtreehouse.com/building-custom-controls-for-html5-videos}
}
\seealso{
  \code{\link[reports]{slidify_layouts}},
  \code{\link[reports]{js_copy}}
}
\keyword{video}

