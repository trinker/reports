\name{cap}
\alias{cap}
\alias{caption_figure}
\alias{caption_table}
\title{Captions for Figures and Tables}
\usage{
cap(type = 1, label, caption, style = "margin-bottom: 3em;",
  copy2clip = interactive(), print = FALSE, spaces = 2)

caption_figure(label, caption, style = "margin-bottom: 3em;",
  copy2clip = interactive(), print = FALSE)

caption_table(label, caption, style = "margin-top: 4em;",
  copy2clip = interactive(), print = FALSE, spaces = 2)
}
\arguments{
  \item{type}{Either "figure" or "table" or accompanying
  numeric reference: figure = 1, table = 2.}

  \item{label}{A hyperlink reference label.}

  \item{caption}{A caption to place below/above a
  figure/table.}

  \item{style}{Additional style elements to be passed to
  the html \code{p} tag.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}

  \item{spaces}{An integer value dictating the number of
  lines to skip after the table caption header.}
}
\value{
Returns a character vector of a caption.
}
\description{
\code{caption} - Keep track of numbering for table and
figure captions.

\code{caption_figure} - Keep track of numbering for figure
captions.

\code{caption_table} - Keep track of numbering for table
captions.
}
\examples{
caption_figure("fig1", "A random figure.")
caption_table("tab1", "A table")
}
\keyword{caption}

