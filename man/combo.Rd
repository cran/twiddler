\name{combo}
\alias{combo}
\title{Combobox to manipulate a string variable by offering a list of alternative expressions}
\usage{
  combo(..., list = NULL, label = as.character(NA))
}
\arguments{
  \item{...}{At least two alternative R expressions to
  choose from.}

  \item{list}{A list of arguments to append to the contents
  of ... .}

  \item{label}{The text label of the combobox.}
}
\value{
  An object of class \code{twiddlerControl}.
}
\description{
  \code{combo} creates a combo \code{twiddlerControl} for
  selecting elements from a list of alternative expressions
  as substitutions for a variable.
}
\seealso{
  \code{\link{twiddle}}
}

