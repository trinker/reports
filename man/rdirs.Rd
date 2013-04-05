\name{rdirs}
\alias{rdirs}
\title{Recursive Directory Creation}
\usage{
  rdirs(..., path = getwd(), sep = "_", pad.num = TRUE,
    text.only = FALSE)
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

  \item{text.only}{logical.  If TRUE rdirs does not create
  the directories, but only returns the names.  This allows
  the names to be passed to \code{new_report} and
  \code{presentation}.}
}
\value{
  Generates recursive sub directories.  Invisibly returns
  the names of the sub-directories.
}
\description{
  Allows the user to input pieces of directory names to
  quickly generate multiple sub-directories with similar
  names nested in the same directory.
}
\examples{
## fx <- folder(delete_me)
## rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6))
rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
## rdirs(session, 1:12, seq(as.Date("2000/1/1"), by = "month", length.out = 12))

x <- rdirs(admin, 1:15, c("d", "f", "w"), c(1, 4, 6), text.only = TRUE)
## lapply(x, new_report)
## delete(fx)
}
\seealso{
  \code{\link[reports]{folder}}, \code{delete},
  \code{\link[base]{dir.create}}
}
\keyword{directory,}
\keyword{file,}
\keyword{folder}

