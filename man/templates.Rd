\name{templates}
\alias{templates}
\title{View Templates}
\usage{
  templates(print.cons = TRUE, names = TRUE)
}
\description{
  View all the reports package templates.
}
\examples{
\dontrun{
templates()
templates(FALSE)
new_report("new", templates(FALSE)[4])
}
}

