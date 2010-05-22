\name{knob}
\alias{knob}
\title{Knob to manipulate a real valued variable...}
\usage{knob(lim=c(0, 1), res=0.01, label=as.character(NA))
}
\description{Knob to manipulate a real valued variable}
\details{\code{knob} creates a slider \code{twiddlerControl} for manipulating
real valued numerical variables.}
\value{A slider \code{twiddlerControl} to be used as an argument
to \code{twiddle}.}
\seealso{\code{\link{twiddle}} and \code{\link{toggle}}}
\arguments{\item{lim}{A vector of two numbers, defining the lower and upper limits
of the slider.}
\item{res}{The resolution or step size of the slider.}
\item{label}{The text label of the slider.}
}

