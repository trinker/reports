\name{FT}
\alias{FT}
\title{Wrap Text With HTML Font Tag}
\usage{
  FT(..., text = "clipboard", copy2clip = TRUE)
}
\arguments{
  \item{\ldots}{1 to 3 arguments of c(`face`, `size` and/or
  `color`).  face accepts one of the following c("arial",
  "arial_black", "comic_sans_ms", "courier", "courier_new",
  "georgia", "helvetica", "impact", "palatino",
  "times_new_roman", "trebuchet_ms", "verdanaBrowse").
  size is any valid whole number.  color can be any R color
  or hex value.}

  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}
}
\value{
  Returns a character vector wrapped with a font tag.
}
\description{
  Wraps text with a font tags. Conveniently detects
  c(`face`, `size` and/or `color`) and creates a font tag
  with the supplied text.
}
\section{Warning}{
  Ligatures parsing is very good, however, these elements
  my be incorrect.  If a warning is thrown check the use of
  "ff", "fi", "fl", "ffi" and "ffl".
}
\examples{
FT(6, text="guy")
FT(6, blue, text="guy")
FT(6, red, times_new_roman, text="guy")
}

