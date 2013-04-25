\name{GQ}
\alias{GQ}
\alias{QQ}
\title{Format Quotes}
\usage{
  GQ(quotes = TRUE, block = TRUE, text = "clipboard",
    copy2clip = TRUE, invisible = FALSE)

  QQ(text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{quotes}{logical or c(\code{l}, \code{r}, \code{L},
  \code{R}, \code{left} or \code{right}).  If \code{TRUE}
  LaTeX style quotes (2 backticks and two single quotes)
  are wrapped around the text.  If (\code{l}, \code{L} or
  \code{left}) left ticks only are used. If (\code{r},
  \code{R} or \code{right}) right ticks only are used.}

  \item{block}{logical.  If \code{TRUE} LaTeX block quote
  code tags are used instead of the backticks and single
  quotes.}

  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{invisible}{logical.  If TRUE will jot print to the
  console.}
}
\value{
  \code{GQ} - Returns formatted text for use with LaTeX
  documents.

  \code{GQ} - Returns formatted text for use with
  notes.xslx or notes.csv.
}
\description{
  \code{GQ} - Tool to grab quote and format the text taken
  from articles for LaTeX. Combines multiple stringed text
  into one string.  Removes non ascii characters and
  hyphens.

  \code{QQ} - Tool to grab a quote and format the text
  taken from articles for use in the directory notes.xslx
  or notes.csv.  Combines multiple stringed text into one
  string.  Removes non ascii characters and hyphens.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}

