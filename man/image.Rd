\name{IM2}
\alias{IM2}
\alias{IS}
\alias{IW}
\title{Convert path/url to HTML Image Tag}
\usage{
  IM2(image = "clipboard", loc = 1, ...)

  IW(path = "clipboard", link = NULL, side = "right",
    width = 540, height = IE(width, round(width/1.5)),
    new_win = TRUE, top = -15, right = 20, left = 20,
    bottom = 0, copy2clip = TRUE, print = FALSE)

  IS(image = "clipboard", ...)
}
\arguments{
  \item{image}{A character vector name of the image.
  Default is to read from the clipboard.}

  \item{loc}{The location of the image: 1-/assets/img and
  2-/figure.}

  \item{\ldots}{Other arguments passed to \code{IM}.}

  \item{side}{The side the image should appear on
  c(\code{"left"}, \code{"right"}).}

  \item{top}{Space between top margin and top of text.}

  \item{right}{Space on the right margin.}

  \item{left}{Space on the left margin..}

  \item{bottom}{Space between bottom margin and bottom of
  text (0 is defualt).}
}
\description{
  \code{IM2} - A wrapper for \code{IM} that sets the base
  path to "assets/img/". This allows the users to just
  specify the image name that resides in one of the
  following directories: 1-"\code{~/assets/img}" or
  2-"\code{~/figure}".

  \code{IW} - Text wrapped images.

  \code{IMF} - A wrapper for \code{IM} that attempts to
  find the image in expected image directories.  Uses the
  first available image matching the image name.  No
  path/url is specified.  IMS stands for `image find`.
}

