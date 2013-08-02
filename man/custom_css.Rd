\name{custom_css}
\alias{custom_css}
\title{Generate Custom css for RStudio + knitr}
\usage{
  custom_css(loc = file.path(getwd(), "REPORT"))
}
\arguments{
  \item{loc}{Path to the report location where the custom
  css should be placed.}
}
\description{
  Generate the components necessary for a custom css for
  use with RStudio and knitr.
}
\details{
  The user must add the custom contents to the custom css
  located in ~/css/style.css
}
\note{
  The user has to source before the custom styles will be
  applied. Open the style.R as the active tab in RStudio,
  and check the `Source on Save` box.
}
\references{
  \url{http://nsaunders.wordpress.com/2012/08/27/custom-css-for-html-generated-using-rstudio/}
}

