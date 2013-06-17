\name{twiddle_print}
\alias{twiddle_print}
\title{Print, then directly flush the output buffer}
\usage{
  twiddle_print(...)
}
\arguments{
  \item{...}{Passed on to \code{\link{print}}}
}
\description{
  This function can be used to "print in realtime" on
  platforms that buffer console output, like on Windows.
}

