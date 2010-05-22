\name{twiddler}
\alias{twiddle}
\title{Interactive manipulation of R expressions...}
\usage{twiddle(expr, ..., eval=TRUE, auto=TRUE)
}
\description{Interactive manipulation of R expressions}
\details{\code{twiddle} automatically creates a GUI for interactively manipulating
variables in an R expression \code{expr}. This GUI will contrain a
control, i.e. a slider or a checkbox, for every unbound variable in \code{expr}.
\code{expr} is re-evaluated every time the user changes a control after binding
the unbound variables in \code{expr} to the new settings of their associated
controls. This allows easy interactive exploration of parameter spaces.}
\value{The last values of the twiddlers as a named list.}
\seealso{\code{\link{knob}}, \code{\link{combo}}, and \code{\link{toggle}}}
\arguments{\item{expr}{The expression to manipulate through GUI controls. A control is
automatically created for every variable in \code{expr} that is not
bound in the caller's environment, unless \code{auto} is set to \code{FALSE}.}
\item{eval}{If set to \code{FALSE}, \code{expr} is not evaluated automatically
each time a control is operated by the user. Instead, an "eval" button is
added to the GUI to trigger manual evaluation.}
\item{auto}{If set to \code{FALSE}, no controls for unbound variables will
be created automatically. Unbound variables in \code{expr} will remain
unbound, unless explicitly bound in the \code{...} parameter.}
\item{...}{Optional configuration information for the controls for unbound
variables in \code{expr}. This information is supplied in the form
\emph{variable_name} \code{=} \emph{twiddlerControl}, ... (e.g.
\code{x = knob(lim = c(0, 10)), b = toggle(default = TRUE)}). If no
\code{twiddlerControl} object is supplied for an unbound variable, a
knob with range [0.0, 1.0] is created as a default. This behaviour
can be suppressed by setting the \code{auto} parameter to \code{FALSE}.}
}
\examples{\dontrun{
twiddle(print(x))
twiddle(print(x), eval = FALSE)
twiddle(print(a && !b), a = toggle(), b = toggle())
twiddle(curve(sin(a * x) + cos(b * x), type = "l", col = "darkblue"),
a = knob(c(0, 0.1), 0.001),
b = knob(c(0, 0.2), 0.001),
auto = FALSE)
twiddle(plot(rnorm(100), type=t), t=combo("p", "l", "b"))
}}

