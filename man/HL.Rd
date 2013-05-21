\name{HL}
\alias{HL}
\title{Highlight HTML Text}
\usage{
  HL(text = "clipboard", color = "yellow",
    copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{text}{A character vector or text copied to the
  clipboard.  Default is to read from the clipboard.}

  \item{color}{A character string taken from R's built-in
  color names or a hexidecimal color.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If \code{FALSE}
  returns to the console.}
}
\description{
  Wraps text with a background color specific font tags.
}
\examples{
cat(HL("Do not trust robots!"), "They are bent on destruction.")
cat(HL("Jake is a cookie scientist,", color="pink"), "an honrable profession.")
}

