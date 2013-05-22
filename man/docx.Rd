\name{tex2docx}
\alias{html2pdf}
\alias{md2docx}
\alias{md2pdf}
\alias{md2tex}
\alias{tex2docx}
\alias{tex2html}
\title{Convert tex/markdown/html to docx/tex/html}
\usage{
  tex2docx(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))

  tex2html(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))

  md2docx(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))

  md2tex(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))

  md2pdf(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))

  html2pdf(in.file = NULL, out.file = NULL,
    path = paste0(getwd(), "/REPORT"),
    bib.loc = getOption("bib.loc"))
}
\arguments{
  \item{in.file}{A character vector of the tex/md file.}

  \item{out.file}{A character vector of the outfile.  If
  \code{"replace"} over writes the original HTML file.
  Default, \code{NULL}, uses the root name of the
  \code{in.file} plus a number 2.}

  \item{path}{The path to where the documents reside/should
  be created. Default is the REPORT directory.  This
  conveniently allows for non paths to be supplied to
  \code{in.file} and \code{out.file} (i.e., just the file
  names).  Paths can be supplied to \code{in.file} and
  \code{out.file} by setting \code{path} to \code{NULL}.}

  \item{bib.loc}{Optional path to a .bib resource.}
}
\description{
  Use \href{pandoc}{http://johnmacfarlane.net/pandoc/} to
  convert tex/markdown/html to docx (or tex/html) for those
  colleagues who use docx.
}
\details{
  The user must have pandoc installed and on their path.
  pandoc can be installed from: \cr
  \href{http://johnmacfarlane.net/pandoc/installing.html}{http://johnmacfarlane.net/pandoc/installing.html}
}
\note{
  \code{md2docx} converts markdown (a .md file) not
  Rmarkdown (.Rmd). Use knitr to knit to HTML first (this
  creates the .md file).
}
\examples{
\dontrun{
DOC <- system.file("extdata/doc_library/apa6.qual_tex/doc.tex",
   package = "reports")
BIB <- system.file("extdata/docs/example.bib", package = "reports")
tex2docx(DOC, file.path(getwd(), "test.docx"), path = NULL, bib.loc = BIB)
}
}

