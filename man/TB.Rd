\name{TB}
\alias{TB}
\title{Wrap text in text boxes.}
\usage{
  TB(text = "clipboard", width = nchar(text),
    text_align = "center", box_align = "left",
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{width}{The width, in characters, to make the box.}

  \item{text_align}{Alingment of text in the box; takes the
  values c(\code{"left"}, \code{"right"}, \code{"top"},
  \code{"middle"}, \code{"bottom"}).}

  \item{box_align}{Alingment of textbox; takes the values
  c(\code{"left"}, \code{"right"}, \code{"top"},
  \code{"middle"}, \code{"bottom"}).}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If \code{FALSE}
  returns to the console.}
}
\value{
  Returns a character vector of an HTML text box tag.
}
\description{
  Wrap text to generate an HTML text box tag.
}
\examples{
TB("I like ice cream!", print=TRUE)
TB("Free cookies for a year!", print=TRUE)
}

