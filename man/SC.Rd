\name{SC}
\alias{SC}
\title{HTML Special Characters}
\usage{
  SC(text, copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns an HTML special character entity.
}
\description{
  A system to produce HTML special characters.
}
\details{
  The general use of \code{SC}is to provide a character
  string of length 2.  The first character is a letter and
  the second is one of the following symbols: \cr \cr
  c("'", "`", ":", "~", "^", "/", "o", ",") \cr \cr Which
  corresponds to: \cr \cr c("acute", "grave", "uml",
  "tilde", "circ", "slash", "ring", "cedil") \cr \cr See
  examples for cases not conforming to this use.
}
\section{Warning}{
  The user can create non-HTML characters with \code{SC}
  that will not be converted (i.e., \code{SC("b~")} would
  yield \code{"&btilde;"} and would not be converted
  appropriately).
}
\examples{
SC("A'")
SC('a\\'')  #can use single quotes with escape
SC("a`")
SC("n~")
SC("o:")
SC("(c)")
SC("(r)")
SC("c|")
SC("o/")
SC("ao")
SC("c,")
SC("p")
SC("P")
SC("E")
SC("Y")
SC("/")
SC("+-")
SC("L")
SC("tm")
SC("S")
}

