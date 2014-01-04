\name{TB}
\alias{TB}
\alias{TB2}
\title{Wrap text in text boxes.}
\usage{
TB(text = "clipboard", ..., col = "white", border = "black",
  padding = 10, lty = "solid", lwd = 1, bor.rad = 5, bold = FALSE,
  font.col = "black", copy2clip = interactive(), print = FALSE)

TB2(text = "clipboard", width = nchar(text), text_align = "center",
  box_align = "left", copy2clip = interactive(), print = FALSE)
}
\arguments{
  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{\ldots}{Other arguments passed to \bold{style} in
  the HTML \code{div} tag.}

  \item{col}{The color(s) to fill or shade the rectangle
  with.}

  \item{border}{The color for rectangle border(s).}

  \item{padding}{The distance (in px) between the text and
  the border.}

  \item{lty}{The line type for borders (either
  \code{"solid"} or \code{"dashed"}).}

  \item{lwd}{The line width (in px) for borders and
  shading.}

  \item{bold}{logical.  If \code{TRUE} the font will be
  boldfaced.}

  \item{font.col}{The color of the font.}

  \item{bor.rad}{The degree (in px) to which the corners
  are rounded; 0 results in square corners.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}

  \item{width}{The width, in characters, to make the box.}

  \item{text_align}{Alingment of text in the box; takes the
  values c(\code{"left"}, \code{"right"}, \code{"top"},
  \code{"middle"}, \code{"bottom"}).}

  \item{box_align}{Alingment of textbox; takes the values
  c(\code{"left"}, \code{"right"}, \code{"top"},
  \code{"middle"}, \code{"bottom"}).}
}
\value{
Returns a character vector of an HTML text box tag.
}
\description{
\code{TB} - Generates an HTML text box tag.

\code{TB2} - Generates an HTML writable text box tag.
}
\examples{
TB("I like ice cream!", print=TRUE)
TB("Free cookies for a year!", col = "red", font.col="white", print=TRUE)
TB("Bad Robot!", "font-style:italic", print=TRUE)
TB2("I like ice cream!", print=TRUE)
TB2("Free cookies for a year!", print=TRUE)
}

