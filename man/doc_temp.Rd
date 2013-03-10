\name{doc_temp}
\alias{doc_temp}
\title{Document Template}
\usage{
  doc_temp(temp.name = "newDoc", doc.type = "tex",
    path = getwd())
}
\arguments{
  \item{temp.name}{The name of the template.}

  \item{doc.type}{The type of report that the template will
  contain (\code{"doc", "rnw", "tex" or "web"}).
  \code{"doc"} will contain a .docx document; \code{"rnw"
  and "tex"} contain both \code{doc.rnw}/\code{doc.tex} and
  \code{preamble.tex} files; whereas \code{"web"} contains
  \code{doc.rmd}/.}

  \item{path}{The path to where the project should be
  created.  Default is the current working directory.}
}
\value{
  Creates a document template framework for template
  personal use or submission.
}
\description{
  This function generates a document template for
  submission to the reports package to be inclusion as a
  template in the doc_library.  This function is similar to
  \code{\link[utils]{package.skeleton}}.
}
\details{
  templates must contain the following two items: \itemize{
  \item{DESCRIPTION}{ - A file used to keep track of users
  and package information.  All fields must be filled in.}
  \item{documents}{ - A minimal working document template.}
  \itemize{ \item{If \code{doc.type = rnw}}{ - Must
  contain: doc.rnw (preamble included)} \item{If
  \code{doc.type = tex}}{ - Must contain: doc.tex and
  preamble.tex} \item{If \code{doc.type = rmd}}{ - Must
  contain: doc.rmd} \item{If \code{doc.type = docx}}{ -
  Must contain: doc.docx} } } Additional project files and
  directories can be stored in the \code{inst} directory.
  Files in this directory will be placed in the main
  directoy of the templace created by
  \code{\link[reports]{new_report}}

  To be submitted a .tex template must run in Debian
  TeXLive on Linux Mint and MikTex on Windows.  After a
  template has been tested it can be sent as a .zip file to
  \code{reports.rpackage@gmail.com}.  Please note that the
  template folder/directory name must end in _tex, _rnw,
  _web or _doc to indicate to users type of template.
}

