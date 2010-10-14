## twiddler.r
##
## twiddleR - interactive manipulation of R expressions
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## released under the GPL v2
##

##' Interactive manipulation of R expressions
##'
##' \code{twiddle} automatically creates a GUI for interactively manipulating
##' variables in an R expression \code{expr}. This GUI will contrain a
##' control, i.e. a slider or a checkbox, for every unbound variable in \code{expr}.
##' \code{expr} is re-evaluated every time the user changes a control after binding
##' the unbound variables in \code{expr} to the new settings of their associated
##' controls. This allows easy interactive exploration of parameter spaces.
##' @note If you use an \code{expr} that generates console output as a side-effect, e.g.
##'   via \code{\link{print}}, and you are running Windows, you might need to use
##'   \code{\link{twiddle_print}} to see the output in realtime.
##' @param expr The expression to manipulate through GUI controls. A control is
##'   automatically created for every variable in \code{expr} that is not
##'   bound in the caller's environment, unless \code{auto} is set to \code{FALSE}.
##' @param eval If set to \code{FALSE}, \code{expr} is not evaluated automatically
##'   each time a control is operated by the user. Instead, an "eval" button is
##'   added to the GUI to trigger manual evaluation.
##' @param auto If set to \code{FALSE}, no controls for unbound variables will
##'   be created automatically. Unbound variables in \code{expr} will remain
##'   unbound, unless explicitly bound in the \code{...} parameter.
##' @param envir The environment in which \code{expr} is to be evaluated. May also be
##'   \code{NULL}, a list, a data frame, a pairlist or an integer as specified to
##'   \code{\link{sys.call}}.
##' @param enclos Relevant when \code{envir} is a (pair)list or a data frame. Specifies
##'   the enclosure, i.e., where R looks for objects not found in \code{envir}. This can
##'   be \code{NULL} (interpreted as the base package environment) or an environment.
##' @param ... Optional configuration information for the controls for unbound
##'   variables in \code{expr}. This information is supplied in the form
##'   \emph{variable_name} \code{=} \emph{twiddlerControl}, ... (e.g.
##'   \code{x = knob(lim = c(0, 10)), b = toggle(default = TRUE)}). If no
##'   \code{twiddlerControl} object is supplied for an unbound variable, a
##'   knob with range [0.0, 1.0] is created as a default. This behaviour
##'   can be suppressed by setting the \code{auto} parameter to \code{FALSE}.
##'
##' @return The last values of the twiddlers as a named list.
##'
##' @examples
##' \dontrun{
##' twiddle(print(x))
##' twiddle(print(x), eval = FALSE)
##' twiddle(print(a && !b), a = toggle(), b = toggle())
##' twiddle(curve(sin(a * x) + cos(b * x), type = "l", col = "darkblue"),
##'         a = knob(c(0, 0.1), 0.001),
##'         b = knob(c(0, 0.2), 0.001),
##'         auto = FALSE)
##' twiddle(plot(rnorm(100), type=t), t=combo("p", "l", "b"))
##' }
##' 
##' @seealso \code{\link{knob}}, \code{\link{combo}}, and \code{\link{toggle}}
##' @rdname twiddler
##' @export
twiddle <- function(expr, ...,
                    eval = TRUE, auto = TRUE,
                    envir = parent.frame(),
                    enclos = if(is.list(envir) || is.pairlist(envir))
                               parent.frame() else baseenv()) {
  params <- list(...)
  quotedExpr <- substitute(expr)
  exprString <- shortenString(deparse(quotedExpr)[[1]], 32)
  givenControls <- Filter(function(p) inherits(p, "twiddlerControl"), params)
  givenVars <- names(givenControls)
  unboundExprVars <- as.character(unboundVariables(quotedExpr))
  varsWithoutControls <- setdiff(unboundExprVars, givenVars)
  automaticallyCreatedControls <- if (auto) {
    l <- Map(function(v) knob(), varsWithoutControls)
    names(l) <- varsWithoutControls
    l
  } else list()
  
  controls <- c(givenControls, automaticallyCreatedControls)
  
  if (length(controls) == 0)
    stop("no controls specified or implied")
  if (length(controls) != length(names(controls)) || any(names(controls) == ""))
    stop("every twiddlerControl must be associated with a variable")
  
  dlg <- tktoplevel()
  tkwm.title(dlg, "twiddler")
  tkwm.resizable(dlg, FALSE, FALSE)
  tkgrid(tklabel(dlg, text = exprString, font = tkfont.create(size = 12)), columnspan = 2, pady = c(4, 0))
  
  for (i in 1:length(controls)) {
    controls[[i]]$variable <- names(controls)[[i]]
    if (is.na(controls[[i]]$label)) controls[[i]]$label <- names(controls)[[i]]
  }
  
  controlValues <- list()
  for (k in controls) controlValues[[k$variable]] <- k$init
  
  updateExpr <- function(variable, value) {
    controlValues[[variable]] <<- value
    if (eval)
      eval(eval(substitute(substitute(e, controlValues), list(e = quotedExpr))),
           envir = envir, enclos = enclos)
  }
  
  makeTkControl <- function(k, v) { v; k$controlFactory(dlg, v, k$label, updateExpr) } # force v
  tkControls <- Map(makeTkControl, controls, names(controls))
  
  for (k in tkControls)
    tkgrid(k, columnspan = 2)
  tkEvalButton <- tkbutton(dlg, text = "Eval",
                           command = function() eval(eval(substitute(substitute(e, controlValues),
                                                                     list(e = quotedExpr))),
                                                     envir = envir, enclos = enclos))
  tkCloseButton <- tkbutton(dlg, text = "Close", command = function() tkdestroy(dlg))
  if (eval)
    tkgrid(tkCloseButton, columnspan = 2, sticky = "we", padx = c(4, 4), pady = c(16, 4))
  else
    tkgrid(tkEvalButton, tkCloseButton, sticky = "we", padx = c(4, 4), pady = c(16, 4))
  
  tkwait.window(dlg)
  controlValues
}

##' Knob to manipulate a real valued variable
##' 
##' \code{knob} creates a slider \code{twiddlerControl} for manipulating
##' real valued numerical variables.
##' @param lim A vector of two numbers, defining the lower and upper limits
##'   of the slider.
##' @param res The resolution or step size of the slider.
##' @param label The text label of the slider.
##' @return A slider \code{twiddlerControl} to be used as an argument
##' to \code{twiddle}.
##' @seealso \code{\link{twiddle}} and \code{\link{toggle}}
##' @export
knob <- function(lim = c(0, 1), res = 0.01, label = as.character(NA))
  structure(list(variable = as.character(NA), label = label, init = lim[1],
                 controlFactory = function(tkTop, v, l, updater) {
                   tkscale(tkTop, from = lim[1], to = lim[2], resolution = res, length = 320,
                           orient = "horizontal", label = l,
                           command = function(...) updater(v, as.numeric(...)))
                 }),
            class = c("knob", "twiddlerControl"))

##' Checkbox to manipulate a boolean valued variable
##' 
##' \code{toggle} creates a checkbox \code{twiddlerControl} for manipulating
##' logical variables.
##' @param default The default logical value for the checkbox, \code{TRUE}
##'   means "checked", \code{FALSE} "unchecked".
##' @param label The text label of the checkbox.
##' @return An object of class \code{twiddlerControl}.
##' @seealso \code{\link{twiddle}} and \code{\link{knob}}
##' @export
toggle <- function(default = FALSE, label = as.character(NA))
  structure(list(variable = as.character(NA), label = label, init = default,
                 controlFactory = function(tkTop, v, l, updater) {
                   cbValue <- tclVar(if (default) "1" else "0")
                   tkcheckbutton(tkTop, text = l, variable = cbValue,
                                 command = function() updater(v, "1" == tclvalue(cbValue)))
                 }),
            class = c("toggle", "twiddlerControl"))

##' Combobox to manipulate a variable by offering a list of alternative expressions
##'
##' \code{combo} creates a combo \code{twiddlerControl} for selecting elements from a list
##' of alternative expressions as substitutions for a variable.
##' @param ... At least two alternative R expressions to choose from.
##' @param list A list of arguments to append to the contents of ... .
##' @param label The text label of the combobox.
##' @return An object of class \code{twiddlerControl}.
##' @seealso \code{\link{twiddle}} and \code{\link{knob}}
##' @export
combo <- function(..., list = NULL, label = as.character(NA)) {
  quotedExprs <- if (missing(list))
    (match.call(expand.dots = FALSE))[[2]] # "..." is the 1st parameter, i.e. [[2]] in call
  else
    c(list, (match.call(expand.dots = FALSE))[[2]])
  stopifnot(length(quotedExprs) > 1) # at least two alternative expressions must be given
  exprStrings <- sapply(as.character(quotedExprs), function(s) shortenString(s, 32))
  structure(list(variable = as.character(NA), label = label, init = quotedExprs[[1]],
                 controlFactory = function(tkTop, v, l, updater) {
                   selection <- tclVar()
                   tclvalue(selection) <- exprStrings[[1]]
                   combobox <- ttkcombobox(tkTop, values = exprStrings,
                                           state = "readonly", textvariable = selection)
                   tkcurrent <- function(widget) as.numeric(tcl(widget, "current")) + 1
                   tkbind(combobox, "<<ComboboxSelected>>",
                          function() updater(v, quotedExprs[[tkcurrent(combobox)]]))
                   combobox
                 }),
            class = c("combo", "twiddlerControl"))
}

##' Display information about a twiddler control
##' 
##' @param x A twiddler control
##' @param ... Ignored
##' @return Invisibly returns \code{x}.
##' @method print twiddlerControl
##' @S3method print twiddlerControl
print.twiddlerControl <- function(x, ...) {
  message("Twiddler control:")
  message(" type     = ", class(x)[1])
  message(" label    = ", x$label)
  message(" default  = ", x$default)
  invisible(x)
}

##' Print, then directly flush the output buffer
##'
##' This function can be used to "print in realtime" on platforms that buffer console output,
##' like on Windows.
##' @param ... Passed on to \code{\link{print}}
##' @export
twiddle_print <- function(...) { print(...); flush.console() }

##' Find the unbound variables of an R expression
##'
##' Returns the \emph{unbound variables} of an R expression \code{expr}. The unbound variables
##' are the variables in \code{expr} that are not bound (do not exist) in the environment
##' of the caller of \code{unboundVariables}.
##'
##' @param qexpr The (quoted) R expression to return the unbound variables for.
##' @return The unbound variables of \code{expr}.
unboundVariables <- function(qexpr) {
  ubvrec <- function(qe, env)
    if (length(qe) == 0)
      list()
    ## Ignore direct data frame/list/matrix row/column references
    else if (is.call(qe))
      if ("$" == qe[[1]] && is.name(qe[[2]]) && is.name(qe[[3]]))
        list()      
      else if (("[[" == qe[[1]] || "[" == qe[[1]]) && is.name(qe[[2]]))
        c(Recall(qe[[3]], env), Recall(qe[-c(1,2,3)], env))
      else ## Ignore unbound functions
        Recall(as.list(qe)[-1], env)
    else if (is.list(qe)) # Recurse into argument lists
      c(Recall(qe[[1]], env), Recall(qe[-1], env))
    else if (is.name(qe) && "" != as.character(qe) && !exists(as.character(qe), env, mode="numeric"))
      c(qe) # Add unbound variable to result
    else
      list()
  unique(ubvrec(qexpr, parent.frame(1)))
}

##' Shorten a string if it is longer than a limit
##'
##' @param text The string to shorten.
##' @param limit The length limit.
##' @param cutIndication A string to append to the string if shortened.
##' @return The shortened string.
shortenString <- function(text, limit, cutIndication = "...") {
  if (nchar(text) <= limit)
    text
  else
    paste(substring(text, 1, limit), cutIndication, sep = "")
}
