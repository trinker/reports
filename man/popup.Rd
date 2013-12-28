\name{popup}
\alias{popup}
\title{Create Mouseovers}
\usage{
  popup(presentation = FALSE, rmd.path = NULL)
}
\arguments{
  \item{presentation}{logical.  If \code{TRUE} created
  popup.js documentation in the PRESENTATION directory.
  \code{FALSE} generates the documentation in the REPORT
  directory.}

  \item{rmd.path}{An optional path to a .Rmd file if
  \code{presentation = FALSE}.}
}
\value{
  Generates an assets directory with appropriate javascript
  and css to make mousever popups.
}
\description{
  Generate mousever events for text.
}
\details{
  \code{popup} adds the css/js scripts necessary to create
  hight quality mouseover popup events.  The user must
  become familiar with Nicolas Honing's
  \href{https://github.com/nhoening/popup.js}{popup.js}.
  Currently no shortcut reports package tag generating
  function is offered for popups that utilize popup.js.  To
  view example uses see
  \url{https://github.com/trinker/popup_example}.
}
\note{
  If adding popup.js documents to non-slidify, .Rmd based
  presentations the user may have to manually add: \cr

  \code{<link rel="stylesheet"
  href="./assets/css/pop_style.css" />} \cr \code{<script
  type="text/javascript"
  src="./assets/js/jquery-1.9.1.min.js"></script>} \cr
  \code{<script type="text/javascript"
  src="./assets/js/nhpup_1.1.js"></script>} \cr

  to the document.
}
\examples{
\dontrun{
popup()
}
}
\references{
  \url{https://github.com/nhoening/popup.js}
}
\keyword{mouseover}
\keyword{popup}

