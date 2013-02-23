\name{ca}
\alias{ca}
\title{Format Text Lines to LaTeX List}
\usage{
  ca(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a charcter vector every word capitalized.
}
\description{
  Itemizes lines of text into a LaTeX list.
}
\details{
  This capitalizes every word of a string.
}

