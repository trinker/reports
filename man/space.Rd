\name{HS}
\alias{HS}
\alias{VS}
\title{Insert HTML Space}
\usage{
  HS(n = 1, copy2clip = TRUE)

  VS(n = 1, copy2clip = TRUE, print = FALSE)
}
\arguments{
  \item{n}{Number of spaces to insert.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{print}{logical.  If TRUE \code{\link[base]{cat}}
  prints the output to the console.  If FALSE returns to
  the console.}
}
\description{
  Insert n iterations of HTML spacing into a document.
}
\details{
  \code{HS} returns horizontal space (using \code{&nbsp;})
  and \code{VS} returns vertical space (using
  \code{</br>}).
}
\examples{
paste0("reports", HS(10), "end")
paste0("reports", VS(4, print = TRUE), "end")
}

