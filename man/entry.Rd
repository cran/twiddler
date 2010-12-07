\name{entry}
\alias{entry}
\title{Text entry field to manipulate a string variable...}
\usage{entry(default="", label=as.character(NA), length=38, eval="key")
}
\description{Text entry field to manipulate a string variable}
\details{\code{entry} creates a text entry field \code{twiddlerControl} for manipulating a
string variable. The variable is updated on each keypress or when the input focus enters
or leaves the text field. See the \code{eval} parameter for details.}
\value{An object of class \code{twiddlerControl}.}
\seealso{\code{\link{twiddle}}, \code{\link{toggle}}, \code{\link{combo}},
and \code{\link{knob}}}
\arguments{\item{default}{The default string to display in the entry, default to \code{""}.}
\item{label}{The text label of the entry.}
\item{length}{The length of the entry in characters, defaults to \code{38}.}
\item{eval}{When to update the string variable manipulated by this entry. Possible
values for this parameter are: \code{"key"} - update on every keystroke (the default),
\code{"focus"} - update when the entry loses focus, or
\code{"return"} - update when the return key is pressed.}
}

