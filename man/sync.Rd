\name{sync}
\alias{sync}
\alias{sync_all}
\alias{sync_img}
\alias{sync_rnp}
\title{File Syncing}
\usage{
sync(dir1, dir2, one_way = FALSE, recursive = TRUE, ...)

sync_img(dir1 = QP("PRESENTATION/figure"),
  dir2 = QP("PRESENTATION/assets/img"), ...)

sync_rnp(dir1 = QP("REPORT/figure"), dir2 = QP("PRESENTATION/figure"), ...)

sync_all()
}
\arguments{
  \item{dir1}{The path to the first directory.}

  \item{dir2}{The path to the second directory.}

  \item{one_way}{logical.  If \code{TRUE} the contents of
  dir2 be copied to dir1, however, dir2 contents will not
  change.  If \code{FALSE} both dir1 and dir2 files will be
  copied to other (dir1 and dir2 will contain identical
  contents).}

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
files in \file{~/PRESENTATION/figure} and \cr
\file{~/PRESENTATION/assets/img}.

\code{sync_rnp} - A wrapper for sync to easily sync the
files in \file{~/REPORT/figure} and
\file{~/PRESENTATION/figure}.

\code{sync_all} - A wrapper for sync to easily sync all
files between \file{~/REPORT/figure},
\file{~/PRESENTATION/figure}, and
\file{~/PRESENTATION/assets/img}.
}
\seealso{
\code{\link[base]{file.copy}}
}

