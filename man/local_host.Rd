\name{local_host}
\alias{local_host}
\title{Generate Local Host Bash/Batch}
\usage{
  local_host(out.dir = file.path(getwd(), "PRESENTATION"))
}
\arguments{
  \item{file}{The path to the presentation directory.}
}
\value{
  Creates a .sh/.bat file that opens a local host to that
  directory.
}
\description{
  Create a .sh/.bat file to open a presentation with a
  local host.
}
\details{
  The user must have \href{http://www.python.org/}{Python}
  installed and on their path to be able to utilize this
  function.  For more details see:
  \url{http://www.youtube.com/watch?v=VlqpiKHz7Gw&feature=youtu.be}.
}
\note{
  The .sh/.bat file can not be opened from within RStudio.
}
\examples{
## local_host()
}
\author{
  Dason Kurkiewicz and Tyler Rinker
  <tyler.rinker@gmail.com>
}
\keyword{localhost}

