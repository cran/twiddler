\name{knob}
\alias{knob}
\title{Knob to manipulate a real valued variable}
\usage{
  knob(lim = c(0, 1), res = 0.01, default = lim[1],
    label = as.character(NA),
    ticks = abs(lim[2] - lim[1])/4, indicator = TRUE,
    length = 320)
}
\arguments{
  \item{lim}{A vector of two numbers, defining the lower
  and upper limits of the slider.}

  \item{res}{The resolution or step size of the slider.}

  \item{default}{The default numeric value D for the
  slider.  lim[1] <= D <= lim[2] must hold.}

  \item{label}{The text label of the slider.}

  \item{ticks}{The tick interval, defaults to
  \code{abs(lim[2] - lim[1]) / 4}. Set this to \code{0} to
  suppress drawing of ticks altogether.}

  \item{indicator}{Whether to show the current value as an
  indicator above the slider's handle, defaults to
  \code{TRUE}.}

  \item{length}{The length of the slider in pixels, default
  to \code{320}.}
}
\value{
  A slider \code{twiddlerControl} to be used as an argument
  to \code{twiddle}.
}
\description{
  \code{knob} creates a slider \code{twiddlerControl} for
  manipulating real valued numerical variables.
}
\seealso{
  \code{\link{twiddle}}
}

