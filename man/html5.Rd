\name{html5}
\alias{html5}
\title{Convert md to HTML5 Slides}
\usage{
  html5(in.file = NULL, out.file = NULL,
    ref.page = "References", refs.font.size = 14,
    path = paste0(getwd(), "/PRESENTATION"))
}
\arguments{
  \item{in.file}{A character vector of the md file.}

  \item{out.file}{A character vector of the outfile.  If
  \code{"replace"} over writes the original HTML file.
  Default, \code{NULL}, uses the root name of the
  \code{in.file} plus a number 2.}

  \item{ref.page}{The title of the reference page (adds
  hanging indent and reduces font size).  If \code{NULL}
  references slide will not be adjusted. If reference title
  is not found a warning will print.}

  \item{refs.font.size}{The font size to make the
  references.}

  \item{path}{The path to where the documents reside/should
  be created. Default is the PRESENTATION directory.  This
  conveniently allows for non paths to be spplied to
  \code{in.file} and \code{out.file}.  Paths can be
  supplied to \code{in.file} and \code{out.file} by setting
  \code{path} to \code{NULL}.}
}
\description{
  Use \href{pandoc}{http://johnmacfarlane.net/pandoc/} to
  convert md to HTML5 slides.
}
\details{
  The user must have pandoc installed and on their path.
  pandoc can be instaleld from: \cr
  \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
}
\examples{
\dontrun{
#Run after running knitr on an Rmd file
html5()  #assumes location of html file out of the box
}
}

