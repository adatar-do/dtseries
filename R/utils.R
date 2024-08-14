#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


# T-Pipe operator
#
#' The T-pipe operator is similar to the pipe operator, but it returns the value
#' of the left-hand side expression instead of the result of the right-hand side
#' function. It is useful for side effects, such as printing or plotting.
#' See \code{\link[magrittr]{\%T>\%}} for details.
#'
#' @name %T>%
#' @rdname tpipe
#' @keywords internal
#' @export
#' @importFrom magrittr %T>%
#' @usage lhs \%T>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The value of `lhs`.
NULL


#' := assigment
#'
#'
#' @name :=
#' @rdname dyn-dots
#' @keywords internal
#' @export
#' @importFrom rlang :=
NULL
