\name{sync}
\alias{sync}
\alias{sync_all}
\alias{sync_img}
\alias{sync_rnp}
\title{File Syncing}
\usage{
  sync(dir1, dir2, one_way = FALSE, recursive = TRUE, ...)

  sync_img(dir1 = file.path(getwd(), "PRESENTATION", "figure"),
    dir2 = file.path(getwd(), "PRESENTATION", "assets", "img"),
    ...)

  sync_rnp(dir1 = file.path(getwd(), "REPORT", "figure"),
    dir2 = file.path(getwd(), "PRESENTATION", "figure"),
    ...)

  sync_all()
}
\arguments{
  \item{dir1}{The path to the first directory.}

  \item{dir2}{The path to the second directory.}

  \item{one_way}{logical.  If TRUE the contents of dir2 be
  copied to dir1, however, dir2 contents will not change.
  If FALSE both dir1 and dir2 files will be copied to other
  (dir1 and dir2 will contain identical contents).}

  \item{recursive}{logical. Should directories and contents
  be recursively copied?}

  \item{\ldots}{For the \code{sync} function \ldots are
  other arguments passed to \code{\link[base]{file.copy}}.
  For \code{sync_img} and \code{sync_rnp} \ldots are
  additional arguments passed to \code{sync}.}
}
\value{
  Syncs files between directories.
}
\description{
  \code{sync} - Sync files between directories.

  \code{sync_img} - A wrapper for sync to easily sync the
  files in ~/PRESENTATION/figure and
  ~/PRESENTATION/assets/img.

  \code{sync_rnp} - A wrapper for sync to easily sync the
  files in ~/REPORT/figure and ~/PRESENTATION/figure.

  \code{sync_rnp} - A wrapper for sync to easily sync all
  files between ~/REPORT/figure, ~/PRESENTATION/figure, and
  ~/PRESENTATION/assets/img.
}
\seealso{
  \code{\link[base]{file.copy}}
}

