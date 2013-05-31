\name{local_rjs}
\alias{local_rjs}
\title{View reveal.js Slide Notes Locally}
\usage{
  local_rjs(reveal.html = NULL,
    revealjs.loc = getOption("revealjs.loc"))
}
\arguments{
  \item{reveal.html}{The location of the
  \href{https://github.com/hakimel/reveal.js/}{reveal.js}
  created html document. Default \code{NULL} will use the
  first available html file in the PRESENTATION directory.}

  \item{revealjs.loc}{}
}
\description{
  Reconfigures the HTML output of a slidify reveal.js
  presentation for use with a reveal.js full install.  This
  allows the user to view slide notes without an Internet
  connection (i.e., local host is used as the server).
}
\details{
  slidify generated reveal.js presentations are not
  configured for use with a full install revealjs.  This
  means that slide notes are not available for local use.
  \code{local_rjs} reconfigures the html output and
  optionally places the file in the user's reveal.js full
  install directory.  Additionally the style.css from
  reports is copied to the css directory in the reveal.js
  full install and named reports.css.

  For maximum efficiency configure the
  \code{\link[base]{options}} for \code{reveraljs.loc} in
  their \code{.Rprofile}.
}
\examples{
## local_rjs()
}
\seealso{
  \href{https://github.com/hakimel/reveal.js/}{Installation
  section of reveal.js GitHub}
}

