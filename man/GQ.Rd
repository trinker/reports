\name{GQ}
\alias{GQ}
\title{Format Quotes}
\usage{
  GQ(quotes = TRUE, block = TRUE, text = "clipboard",
    copy2clip = TRUE)
}
\arguments{
  \item{quotes}{logical.  If \code{TRUE} LaTeX style quotes
  (2 backticks and two single quotes) are wrapped around
  the text.}

  \item{block}{If \code{TRUE} LaTeX block quote code tags
  are used instead of the backticks and single quotes.}

  \item{text}{character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a charcter vector with LaTeX formatted text.
}
\description{
  Tool to format text taken from articles for LaTeX.
  Combines multiple s tringed text into one string.
  Removes non ascii characters and hyphens.
}
\details{
  This function formats text for use with LaTeX documents.
}

