\name{new_report}
\alias{new_report}
\title{Report Template}
\usage{
  new_report(report = "report",
    bib.loc = getOption("bib.loc"),
    name = getOption("name_reports"), path = getwd())
}
\arguments{
  \item{name}{A character vector of the project name.}

  \item{bib.loc}{Optional path to a .bib resource.}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}
}
\value{
  Creates a report template.
}
\description{
  Generate a report/paper template to increase efficiency.
}

