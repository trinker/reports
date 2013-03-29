\name{rdirs}
\alias{rdirs}
\title{Recursive Directory Creation}
\usage{
  rdirs(..., path = getwd(), sep = "_", pad.num = TRUE)
}
\arguments{
  \item{\ldots}{The pieces of the names to put together.
  \code{rdirs} will use R's recylcing rule with different
  length vectors.}

  \item{path}{A character vector specifying the root
  directory path.}

  \item{sep}{A character string to separate the terms.}

  \item{pad.num}{logical.  If TRUE numbers will be padded
  with leading zeros (detects numeric strings supplied
  using the colon(\code{:}) operator or combine (\code{c(})
  function.}
}
\value{
  Generates recursive sub directories.
}
\description{
  Allows the user to input pieces of directory names to
  quickly generate multiple directories with similar names
  nested in the same directory.
}
\examples{
\dontrun{
rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6))
rdirs(session, 1:12, seq(as.Date("2000/1/1"), by = "month", length.out = 12))
}
}
\seealso{
  \code{\link[reports]{folder}},
  \code{\link[base]{delete}},
  \code{\link[base]{dir.create}}
}
\keyword{directory,}
\keyword{file,}
\keyword{folder}

