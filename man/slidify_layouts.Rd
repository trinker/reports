\name{slidify_layouts}
\alias{slidify_layouts}
\title{Generate Extra Slidify Layouts}
\usage{
  slidify_layouts(path = QP("assets/layouts"))
}
\arguments{
  \item{path}{The path to the layout directory where the
  html files should be generated.  Default is
  \file{~assets/layouts}.}
}
\value{
  Creates the layout html files to extend the slidify slide
  layout framework.  Currently \code{slidify_layouts}
  generates a carousel.html, thankyou.html, and
  youtube.html.  See the references for additional
  information.
}
\description{
  A collection of additional slidify slide layouts to
  extend the slidify framework.
}
\note{
  If \code{\link[reports]{new_report}} or
  \code{\link[reports]{presentation}} was utilized to
  generate the slidify presentation directory
  \code{\link[reports]{slidify_layouts}} has already
  created the extra slidify slide layouts in:
  \file{~PRESENTATION/assets/layouts}
}
\references{
  INSERT LINK TO slidify tricks and tips section
}

