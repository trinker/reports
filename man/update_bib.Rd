\name{update_bib}
\alias{update_bib}
\title{Update bib File}
\usage{
  update_bib(report = getwd(),
    bib.loc = getOption("bib.loc"))
}
\arguments{
  \item{report}{Path to the report project.}

  \item{bib.loc}{Optional path to a .bib resource.}
}
\value{
  Updates bib from master/global .bib file.
}
\description{
  Updates the report directory .bib file with a
  global/master .bib file.
}

