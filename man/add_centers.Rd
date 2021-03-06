% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-centers.R
\name{add_centers}
\alias{add_centers}
\title{Add centroids manually}
\usage{
add_centers(
  centers,
  xmap,
  x = NULL,
  y = NULL,
  phase = if (is.null(x)) NULL else paste0("P", seq_along(x)),
  i = data.frame(x, y, phase),
  ...,
  saveas = "center_add.csv"
)
}
\arguments{
\item{centers}{c-by-p matrix returned by \code{\link[=find_centers]{find_centers()}} or by manually;
c clusters and p features.
Used to guess initial centers (or centroids) of clusters.
A value returned by , typically \code{\link{data.frame}} or \code{\link{matrix}},
indicating initial guess centers (or centroids) or clusters.
See \code{\link[=find_centers]{find_centers()}}.}

\item{xmap}{An object generated by \code{\link[=read_xmap]{read_xmap()}}}

\item{x}{
A n-by-p training data matrix; n observations and p features. Used to train the
classifier.
}

\item{y}{Integer vectors specifying x- and y-coordinates of pixels of mapping data.
In other words, x- and y- corresponds to n-th column and row of a matrix.}

\item{phase}{Selected ones are referenced to detect outliers. Default selects everything.
Tidy selection is available. For example \code{c(Si, Ti)} selects them, and
\code{c(-Si, -Ti)} selects everything except them.}

\item{i}{A data frame with clumns \code{x}, \code{y}, and \code{phase}.
\code{i} have priority to \code{x}, \code{y}, and \code{phase}.`}

\item{...}{Ignored}

\item{saveas}{File name to save result. \code{FALSE} if not saving.}
}
\description{
Add centroids manually by picking X-ray counts of selected pixels in X-ray maps.
}
\seealso{
\code{\link[=pick]{pick()}}, \code{\link[=find_centers]{find_centers()}}
}
