\name{reveal.js}
\alias{reveal.js}
\title{Convert md to HTML5 reveal.js}
\usage{
  reveal.js(theme = "default", transition = "default",
    in.file = NULL, out.dir = path,
    ref.page = "References", refs.cex = 15,
    path = paste0(getwd(), "/PRESENTATION"), hi.cex = 25)
}
\arguments{
  \item{theme}{possible reveal.js themes: c(\code{"sky"},
  \code{"beige"}, \code{"simple"}, \code{"serif"},
  \code{"night"}, \code{"default"}).}

  \item{transition}{possible reveal.js transitions:
  c(\code{"default"}, \code{"cube"}, \code{"page"},
  \code{"concave"}, \code{"zoom"}, \code{"linear"},
  \code{"fade"}, \code{"none"}).}

  \item{in.file}{A character vector of the md file.}

  \item{out.dir}{A character vector of the out directory.
  Default uses the root name and outputs to a directory
  called reveal_js.}

  \item{ref.page}{The title of the reference page (adds
  hanging indent and reduces font size).  If \code{NULL}
  references slide will not be adjusted. If reference title
  is not found a warning will print.}

  \item{refs.cex}{The font size to make the references.}

  \item{path}{The path to where the documents reside.
  Default is the PRESENTATION directory.  This conveniently
  allows for non paths to be spplied to \code{in.file} and
  \code{out.file}.  Paths can be supplied to \code{in.file}
  and \code{out.file} by setting \code{path} to
  \code{NULL}.}

  \item{hi.cex}{The font size to make the hanging indent
  coded text if \code{hi} code chunk is used.}
}
\description{
  Uses \href{pandoc}{http://johnmacfarlane.net/pandoc/} to
  convert md to HTML5
  \href{http://lab.hakim.se/reveal-js/}{reveal.js} slides
  and provides minor modifications (e.g., embedded youtube
  and hanging indent text, etc.).
}
\details{
  Outputs a directoy, revel_js, with an index.html and
  necessary
  \href{http://lab.hakim.se/reveal-js/}{reveal.js} files.
  The user must have Pandoc installed and on their path.
  Pandoc can be instaleld from: \cr
  \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
}
\section{Code Chunks}{
  The following convience code chunks are implemented:
  \enumerate{ \item{\bold{bg-} - Place after a slide title
  to change background color.  Currently takes
  \code{bg-soothe}, \code{bg-blackout} and
  \code{bg-alert}.} \item{\bold{frag-} - Using a dash/space
  (- ) has the usual effect of text appearing however
  special effects can be applied to text by using the
  dash/space followed by the text and followed by
  \code{[[[]]]=frag-} and last a fragment style.  Valid
  styles include: \code{grow}, \code{shrink},
  \code{roll-in}, \code{fade-out}, \code{highlight-red},
  \code{highlight-green}, or \code{highlight-blue}.}
  \item{\bold{hi} - Wraping text with this code chunk will
  result in hangingin indentation.  Use \code{hi.cex} to
  control the font size of the text.} \item{\bold{notes} -
  Wrap presenter notes that work when slides are uploded to
  the Internet. Press "s" to get the speaker notes window.}
  \item{\bold{small} - Wrap text to produce small font.}
  \item{\bold{sud} & \bold{eud}- Wrap a group of slides to
  give the nested up and down capabilities. start-up-down
  (sud) goes directly above the begining side in the .Rmd
  file, end-up-down (eud) goes directly at the end of the
  last slide in the nested group, however there should be a
  space between text and this code tag.} \item{\bold{yt} -
  Wrap a youtube url or tag to embed a youtube video.} }
  Code chunks use the following form:
  \code{\bold{[[[text]]]=code.tag}} (e.g.,
  \bold{[[[cokNUTGtoM4]]]=yt} embeds a youtube video.
  Currently this is a convenience feature that may have
  unexpected results and may need additional tweaking
  within the html output.  When using embedded youtube,
  slide titles and text are ignored but may effect the
  spacing of the player. User additions are welcomed.
}
\examples{
\dontrun{
#Run after running knitr on an Rmd file
reveral.js()  #assumes location of html file out of the box
}
}

