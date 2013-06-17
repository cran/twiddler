\name{toggle}
\alias{toggle}
\title{Checkbox to manipulate a logical variable}
\usage{
  toggle(default = FALSE, label = as.character(NA))
}
\arguments{
  \item{default}{The default logical value for the
  checkbox, \code{TRUE} means "checked", \code{FALSE}
  "unchecked".}

  \item{label}{The text label of the checkbox.}
}
\value{
  An object of class \code{twiddlerControl}.
}
\description{
  \code{toggle} creates a checkbox \code{twiddlerControl}
  for manipulating logical variables.
}
\seealso{
  \code{\link{twiddle}}
}

