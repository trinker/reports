\name{html5}
\alias{html5}
\title{Convert md to HTML5 Slides}
\usage{
  html5(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/PRESENTATION"))
}
\arguments{
  \item{in.file}{A character vector of the md file.}

  \item{out.file}{A character vector of the outfile.  If
  NULL over writes the original HTML file.}

  \item{path}{The path to where the project should be
  created.  Default is the PRESENTATION directory.}
}
\value{
  Creates a report template.
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

