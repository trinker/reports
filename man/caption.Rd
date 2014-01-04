\name{caption}
\alias{caption}
\alias{caption_figure}
\alias{caption_table}
\title{Captions for Figures and Tables}
\usage{
caption(type = 1, label, caption, copy2clip = interactive(),
  print = FALSE)

caption_figure(label, caption, copy2clip = interactive(), print = FALSE)

caption_table(label, caption, copy2clip = interactive(), print = FALSE)
}
\arguments{
  \item{type}{Either "figure" or "table" or accompanying
  numeric reference: figure = 1, table = 2.}

  \item{label}{A hyperlink reference label.}

  \item{caption}{A caption to place below/above a
  figure/table.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If \code{TRUE}
  \code{\link[base]{cat}} prints the output to the console.
  If \code{FALSE} returns to the console.}
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
\dontrun{
caption_figure("fig1", "A random figure.")
caption_table("tab1", "A table")
}
}
\keyword{caption}

