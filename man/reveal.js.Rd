\name{reveal.js}
\alias{reveal.js}
\title{Convert md to HTML5 reveal.js}
\usage{
  reveal.js(theme = "default", transition = "default",
    in.file = NULL, out.dir = path,
    ref.page = "References", refs.cex = 15,
    path = file.path(getwd(), "PRESENTATION"),
    figure.dir = "figure", hi.cex = 25)
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
  allows for non paths to be supplied to \code{in.file} and
  \code{out.file}.  Paths can be supplied to \code{in.file}
  and \code{out.file} by setting \code{path} to
  \code{NULL}.}

  \item{figure.dir}{The name of the figures directory where
  figures are stored. This should be in the presentation
  folder.  By default it is called figure.}

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
  Outputs a directory, revel_js, with an index.html and
  necessary
  \href{http://lab.hakim.se/reveal-js/}{reveal.js} files.
  The user must have Pandoc installed and on their path.
  Pandoc can be installed from: \cr
  \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
}
\section{Code Chunks}{
  The following convenience code chunks are implemented:
  \enumerate{ \item{\bold{bg-} - Place after a slide title
  to change background color.  Currently takes
  \code{bg-soothe}, \code{bg-blackout}, \code{bg-alert},
  \code{ red }, \code{ orange }, \code{ yellow }, \code{
  green }, \code{ blue }, \code{ indigo }, \code{ violet },
  or \code{ brown }.} \item{\bold{frag-} - Using a
  dash/space (- ) has the usual effect of text appearing
  however special effects can be applied to text by using
  the dash/space followed by the text and followed by
  \code{[[[]]]=frag-} and last a fragment style.  Valid
  styles include: \code{grow}, \code{shrink},
  \code{roll-in}, \code{fade-out}, \code{highlight-red},
  \code{highlight-green}, or \code{highlight-blue}.}
  \item{\bold{hi} - Wrapping text with this code chunk will
  result in hanging indentation.  Use \code{hi.cex} to
  control the font size of the text.} \item{\bold{notes} -
  Wrap presenter notes that work when slides are uploaded
  to the Internet. Press "s" to get the speaker notes
  window.} \item{\bold{small} - Wrap text to produce small
  font.} \item{\bold{sud} & \bold{eud}- Wrap a group of
  slides to give the nested up and down capabilities.
  start-up-down (sud) goes directly above the beginning
  side in the .Rmd file, end-up-down (eud) goes directly at
  the end of the last slide in the nested group, however
  there should be a space between text and this code tag.}
  \item{\bold{yt} - Wrap a youtube url or tag to embed a
  youtube video.} } Code chunks use the following form:
  \bold{[[[text]]]=code.tag} (e.g.,
  \bold{[[[cokNUTGtoM4]]]=yt} embeds a youtube video.
  Currently this is a convenience feature that may have
  unexpected results and may need additional tweaking
  within the html output.  When using embedded youtube,
  slide titles and text are ignored but may effect the
  spacing of the player. User additions are welcomed.

  reports based code chunks are for convenience.  For more
  control many HTML tags work with Rmarkdown.  The output
  code can also be manipulated for finer control.
}

\section{Warning}{
  reveal.js is deprecated and will be removed from reports
  in a version subsequent to version 0.2.0.  It is
  recomended for the user to become familiar with the
  slidify approach to html5 presentations as this will be
  the framework used by the reports package.
}
\examples{
\dontrun{
#Run after running knitr on an Rmd file
reveal.js()  #assumes location of html file out of the box

# An example .Rmd file can be found:
system.file("extdata/docs/example.Rmd", package = "reports")
}
}
\seealso{
  \code{\link[reports]{html5}}
}

