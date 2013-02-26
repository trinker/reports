\name{repo2github}
\alias{repo2github}
\title{Upload a Local Repo to GitHub (WARNING: CURRENTLY NOT FUNCTIONING)}
\usage{
  repo2github(password = "", repo = basename(getwd()),
    user = getOption("github.user"), gitpath = NULL)
}
\arguments{
  \item{password}{GitHub password (character string).}

  \item{user}{GitHub user name (character string).}

  \item{repo}{Character string naming the repo; default
  attempts to use the report project directory name.}

  \item{gitpath}{Path to the location of git.  If
  \code{NULL} \code{repo2github} will attempt to locate the
  path if necessary.}
}
\value{
  Creates GitHub reopository.
}
\description{
  (WARNING: CURRENTLY NOT FUNCTIONING)
}
\details{
  Allows uploading a local repository to GitHub without
  first creating the repository in the clouds.
}
\note{
  To use \code{repo2github} the user must have initiallized
  \code{\link[reports]{new_report}} or
  \code{\link[reports]{presentation}} with a \code{.git}
  file by selecting the argument \code{git = TRUE}.  The
  user will also need to have a
  \href{https://github.com/}{GitHub} site established.
}
\section{GitHub Website}{
  \url{https://github.com/}
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
  SimonO101 of stackoverflow.com and tyler Rinker
  <tyler.rinker@gmail.com>
}
\references{
  \url{http://stackoverflow.com/a/15047013/1000343}
}

