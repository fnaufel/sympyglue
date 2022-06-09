#' Assemble string with interpolated values
#'
#' @description This an extension of \code{\link[glue]{glue}}.
#'
#' Expressions enclosed by `<<` and `>>` will be evaluated as Python (or optionally R) code. The result will be inserted as LaTeX math code (optionally R expressions).
#'
#' Long strings are broken by line and concatenated together. Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#'
#' @param ... Any number of strings (to be concatenated).
#' @param asis If TRUE, output is generated with `cat`; in this case, the function must be called inside a chunk with `results='asis'`. If FALSE, output is generated with \code{\link[knitr]{asis_output}}; in this case, `results='asis'` is not necessary.
#'
#' @return A character vector after interpolation.
#'
#' @author fnaufel
#'
#' @importFrom glue glue
#' @importFrom knitr asis_output
#'
#' @export
#'
m <- function(..., asis = getOption('m_asis', FALSE)) {

  f <- ifelse(asis, cat, knitr::asis_output)

  f(
    glue::glue(
      ...,
      .open = '<<',
      .close = '>>',
      .transformer = sympy_transformer
    )
  )

}


#' Custom transformer
#'
#' @param text Contents between one `<<...>>` pair.
#' @param envir Not used.
#'
#' @return Character vector after evaluation.
#'
#' @author fnaufel
#'
#' @importFrom reticulate py_run_string
#' @importFrom purrr map_chr
#'
sympy_transformer <- function(text, envir) {

  # Comma or period for decimal?
  decimal <- ifelse(
    getOption('OutDec') == ',',
    '"comma"',
    '"period"'
  )

  # Evaluate text in python
  obj <- reticulate::py_run_string(paste0('__m = ', text))
  obj <- obj[['__m']]

  # If list or vector, map latex function; result is a character vector
  if (is.vector(obj)) {
    m_latex <- purrr::map_chr(
      obj,
      ~py$latex(., decimal_separator = decimal)
    )
  } else {
  # If single object, just call latex function
    m_latex <- py$latex(obj, decimal_separator = decimal)
  }

  m_latex

}

