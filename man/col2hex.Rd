\name{col2hex}
\alias{col2hex}
\title{Convert R colors to Hexadecimal}
\usage{
  col2hex(rcolor)
}
\arguments{
  \item{rcolor}{A character string taken from R's built-in
  color names.}
}
\value{
  Returns a character string of converted hexadecimal
  colors.
}
\description{
  Convert R colors() to Hexadecimal.
}
\examples{
col2hex("darkblue")
col2hex(c("darkblue", "red"))
x <- paste0("<hr color=\\"", col2hex("red"), "\\" size=\\"4\\">")
cat(x)
}
\seealso{
  \code{\link[grDevices]{colors}},
  \code{\link[grDevices]{rgb}},
  \code{\link[grDevices]{col2rgb}}
}

