\name{unboundVariables}
\alias{unboundVariables}
\title{Find the unbound variables of an R expression}
\usage{
  unboundVariables(qexpr)
}
\arguments{
  \item{qexpr}{The (quoted) R expression to return the
  unbound variables for.}
}
\value{
  The unbound variables of \code{expr}.
}
\description{
  Returns the \emph{unbound variables} of an R expression
  \code{expr}. The unbound variables are the variables in
  \code{expr} that are not bound (do not exist) in the
  environment of the caller of \code{unboundVariables}.
}

