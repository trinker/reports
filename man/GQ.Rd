\name{GQ}
\alias{GQ}
\title{Format Quotes}
\usage{
  GQ(quotes = TRUE, block = TRUE, text = "clipboard",
    copy2clip = TRUE)
}
\arguments{
  \item{quotes}{logical or c(\code{l}, \code{r}, \code{L},
  \code{R}, \code{left} or \code{right}).  If \code{TRUE}
  LaTeX style quotes (2 backticks and two single quotes)
  are wrapped around the text.  If (\code{l}, \code{L} or
  \code{left}) left ticks only are used. If (\code{r},
  \code{R} or \code{right}) right ticks only are used.}

  \item{block}{If \code{TRUE} LaTeX block quote code tags
  are used instead of the backticks and single quotes.}

  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector with LaTeX formatted text.
}
\description{
  Tool to format text taken from articles for LaTeX.
  Combines multiple stringed text into one string.  Removes
  non ascii characters and hyphens.
}
\details{
  This function formats text for use with LaTeX documents.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}

