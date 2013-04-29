\name{CW}
\alias{CW}
\title{Count Words}
\usage{
  CW(text = "clipboard")
}
\arguments{
  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}
}
\description{
  Tool to count words from text taken from articles.
}
\details{
  This function counts the number of words in a text chunk.
  Useful for determining if block quotes are needed in
  APA6.  While the function accepts vectors the real
  increase in work flow productivity is copying from the
  clipboard.
}
\examples{
CW("I like icecream")
}

