\name{LL}
\alias{LL}
\title{Format Text Lines to LaTeX List}
\usage{
  LL(enumerate = TRUE, text = "clipboard",
    copy2clip = TRUE)
}
\arguments{
  \item{enumerate}{logical.  If \code{TRUE} uses the
  enumerate environment.  If \code{FALSE} itemize is used
  instead.}

  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector with a LaTeX list formatted
  text.
}
\description{
  Itemizes lines of text into a LaTeX list.
}
\details{
  This function formats text for use with LaTeX as a list.
}
\section{Warning}{
  Ligatures are assumed to be "fi", however, these elements
  may be "ff", "fi", "fl", "ffi" or "ffl".
}
\examples{
\dontrun{
x <- readLines(n = 3)
one, two buckle my shoe
three, four close the door
five, six pick up sticks
LL(, x)
LL(FALSE, x)
}
}

