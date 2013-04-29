\name{html5}
\alias{html5}
\title{Convert md to HTML5 Slides}
\usage{
  html5(in.file = NULL, out.file = NULL, type = "dzslides",
    ref.page = "References", refs.cex = 15,
    path = file.path(getwd(), "PRESENTATION"), hi.cex = 25)
}
\arguments{
  \item{in.file}{A character vector of the md file.}

  \item{out.file}{A character vector of the outfile.  If
  \code{"replace"} over writes the original HTML file.
  Default, \code{NULL}, uses the root name of the
  \code{in.file} plus a number 2.}

  \item{type}{A character string of the desired slide type;
  either c(\code{"slidy"} or \code{"dzslides"}).}

  \item{ref.page}{The title of the reference page (adds
  hanging indent and reduces font size).  If \code{NULL}
  references slide will not be adjusted. If reference title
  is not found a warning will print.}

  \item{refs.cex}{The font size to make the references.}

  \item{path}{The path to where the documents reside/should
  be created. Default is the PRESENTATION directory.  This
  conveniently allows for non paths to be supplied to
  \code{in.file} and \code{out.file}.  Paths can be
  supplied to \code{in.file} and \code{out.file} by setting
  \code{path} to \code{NULL}.}

  \item{hi.cex}{The font size to make the hanging indent
  coded text if \code{hi} code chunk is used.}
}
\description{
  Uses \href{pandoc}{http://johnmacfarlane.net/pandoc/} to
  convert md to HTML5
  \href{http://paulrouget.com/dzslides/}{DZSlides} slides
  or slidy and provides minor modifications (e.g., embedded
  youtube and hanging indent text).
}
\details{
  The user must have Pandoc installed and on their path.
  Pandoc can be installed from: \cr
  \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
}
\section{Code Chunks}{
  The following convenience code chunks are implemented:
  \enumerate{ \item{\bold{hi} - Wrapping text with this
  code chunk will result in hanging indentation.  Use
  \code{hi.cex} to control the font size of the text.}
  \item{\bold{yt} - Wrap a youtube url or tag to embed a
  youtube video} } Code chunks use the following form:
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
  \code{html5} demos can be found at
  \url{http://trinker.github.com/reports/examples/}.
}

\section{Warning}{
  html5 is deprecated and will be removed from reports in a
  version subsequent to version 1.3.  It is recomended for
  the user to become familiar with the slidify approach to
  html5 presentation as this will be the framework used by
  the reports package.
}
\examples{
\dontrun{
#Run after running knitr on an Rmd file
html5()  #assumes location of html file out of the box
}
}
\author{
  Ananda Mahto & Tyler Rinker <tyler.rinker@gmail.com>
}
\references{
  \url{http://stackoverflow.com/a/14971683/1000343}
}
\seealso{
  \code{\link[reports]{reveal.js}}
}

