\name{filer}
\alias{filer}
\title{File selection dialog to manipulate a string variable denoting a filename}
\usage{
  filer(default = "", label = as.character(NA),
    length = 32)
}
\arguments{
  \item{default}{The default filename for the file
  selection dialog.}

  \item{label}{The text label of the file selection
  dialog.}

  \item{length}{The length of the entry in characters,
  defaults to \code{38}.}
}
\value{
  An object of class \code{twiddlerControl}.
}
\description{
  \code{filer} creates a file selection
  \code{twiddlerControl} for manipulating string variables
  denoting filenames.
}
\seealso{
  \code{\link{twiddle}}
}

