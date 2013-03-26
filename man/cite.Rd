\name{citeL}
\alias{citeL}
\alias{parenciteL}
\alias{parenciteM}
\alias{posciteL}
\alias{possciteL}
\alias{textciteL}
\alias{textciteM}
\title{Format Quotes with Citations}
\usage{
  citeL(text.loc = NULL, copy2clip = TRUE, citation = TRUE)

  parenciteL(text.loc = NULL, copy2clip = TRUE,
    citation = TRUE)

  textciteL(text.loc = NULL, copy2clip = TRUE,
    citation = TRUE)

  possciteL(text.loc = NULL, copy2clip = TRUE,
    citation = TRUE)

  posciteL(text.loc = NULL, copy2clip = TRUE,
    citation = TRUE)

  parenciteM(text.loc = NULL, width = 70,
    force.block = TRUE, copy2clip = TRUE, citation = TRUE,
    bib.name = "bib")

  textciteM(text.loc = NULL, width = 70,
    force.block = TRUE, copy2clip = TRUE, citation = TRUE,
    bib.name = "bib")
}
\arguments{
  \item{text.loc}{The row number (integer value) from
  notes.xlsx/notes.csv of the text to insert.  The user may
  also input a character string of partial matches of the
  bibkeys (a quasi-author search).  If NULL the interactive
  use allows the user to view the notes.xlsx/notes.csv and
  chose a row number.}

  \item{copy2clip}{logical.  If \code{TRUE} attempts to
  copy the output to the clipboard.}

  \item{citation}{logical.  If TRUE will include the
  formatted citation + the quote.}

  \item{width}{The widths of a block quote output (for the
  markdown family only).}

  \item{force.block}{logical.  If TRUE forces a block less
  than 40 words to be a block quote.}
}
\value{
  Returns a character vector with LaTeX/markdown formatted
  text.
}
\description{
  Citation tools to grab a quote and format the text taken
  from the notes.xlsx/notes.csv for LaTeX with optional
  citation included.  Functions attempt to copy the output
  to the clipboard for easy paste inclusion.
}
\details{
  Each of the functions in the cite family follow a pattern
  of (cite, parencite, textcite, posscite, poscite) prefix
  and (L or M) suffix (note that currently only parencite
  and textcite functions exist for markdown). The cite and
  textcite are in the form of LaTeX commands by the same
  name. posscite and poscite are user defined LaTeX
  function styles that are extensions of the textcite
  command to fit possessive and -s- ending possessives.
  They can be defined as:

  \code{\\newcommand\\posscite[1]{\\citeauthor{#1}'s
  (\\citeyear{#1})}} \cr
  \code{\\newcommand\\poscite[1]{\\citeauthor{#1}'
  (\\citeyear{#1})}} \cr

  The L and M correspond to LaTeX or markdown outputs;
  markdown relies on the \code{knitcitations} package.
}
\section{Note}{
  It is expected that the user maintains
  notes.xlsx/notes.csv with markdown notation (e.g., ** for
  bold type and * for italics).
}

