\name{repo2github}
\alias{repo2github}
\title{Upload a Local Repo to GitHub (WARNING: CURRENTLY NOT FUNCTIONING)}
\usage{
  repo2github(password, project.dir = getwd(),
    repo = basename(getwd()),
    github.user = getOption("github.user"), gitpath = NULL)
}
\arguments{
  \item{password}{GitHub user password (character string).}

  \item{project.dir}{The path to the root directory of the
  report/presentation.}

  \item{repo}{Character string naming the repo; default
  attempts to use the report project directory name.}

  \item{github.user}{GitHub user name (character string).}

  \item{gitpath}{Path to the location of git.  If
  \code{NULL} \code{repo2github} will attempt to locate the
  path if necessary.}
}
\value{
  Creates GitHub repository.
}
\description{
  (WARNING: CURRENTLY NOT FUNCTIONING)
}
\details{
  Allows uploading a local repository to GitHub without
  first creating the repository in the clouds.
}
\note{
  To use \code{repo2github} the user must have initialized
  \code{\link[reports]{new_report}} or
  \code{\link[reports]{presentation}} with a \code{.git}
  file by selecting the argument \code{git = TRUE}.  The
  user will also need to have a
  \href{https://github.com/}{GitHub} site established.
}
\section{GitHub Website}{
  \url{https://github.com/}
}

\section{Warning}{
  The arguments \code{project.dir} and \code{repo} use
  \code{\link[base]{gewtwd}}.  This assumes is the current
  working directoy is the root directory and is done for
  convienence.  The user should ensure that either their
  working directory is the root directory or supply the
  correct root directory/name to these arguments.
}

\section{Suggestion}{
  The user may want to set \code{\link[base]{options}} for
  \code{github.user} in the user's primary
  \code{.Rprofile}.
}
\examples{
\dontrun{
repo2github()
}
}
\author{
  Simon O'Hanlon and Tyler Rinker <tyler.rinker@gmail.com>
}
\references{
  \url{http://stackoverflow.com/a/15047013/1000343}
}

