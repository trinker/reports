\name{is.url}
\alias{is.url}
\title{Check If String is a URL}
\usage{
is.url(x)
}
\arguments{
  \item{x}{A character string.}
}
\value{
Returns a logical evalution as to whether a string is a
url.
}
\description{
Uses a regex approach to check if a string is a url.  This
approach is faster than \code{\link[RCurl]{url.exists}} but
does do the actual verification.
}
\examples{
urls <- c("a", "f/g/h", "www.talkstats.com", "https://github.com/trinker")
is.url(urls)
}
\seealso{
\code{\link[RCurl]{url.exists}}
}
\keyword{url}

