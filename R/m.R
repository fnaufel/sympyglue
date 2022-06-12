#' Assemble string with interpolated values from R or Sympy
#'
#' @description This an extension of \code{\link[glue]{glue}}.
#'
#' Expressions enclosed by `{{` and `}}` will be evaluated as Sympy (or optionally R) code.
#'
#' If evaluated as Sympy code, the result will be inserted as LaTeX.
#'
#' Long strings are broken by line and concatenated together. Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#'
#' @param ... Any number of strings (to be concatenated).
#'
#' @param sep Separator between main strings (`...`).
#'
#'   If `...` is only one string, this has no effect.
#'
#' @param sep_middle Separator between elements of the interpolated character vectors.
#'
#'   If none of the `{{...}}` fields generates vectors with length > 1, this has no effect.
#'
#' @param before Text to be emitted at the beginning. May NOT contain `{{...}}` fields.
#'
#' @param sep_before Separator between elements of `before` (if length > 1).
#'
#' @param after Text to be emitted at the end. May NOT contain `{{...}}` fields.
#'
#' @param sep_after Separator between elements of `after` (if length > 1).
#'
#' @param sep_blocks Separator between `before`, `middle` and `after`.
#'
#' @param use_cat If TRUE, output is generated with \code{\link[stringr]{str_c}}; in this case, the function must be called inside a chunk with `results='asis'`.
#'
#'   If FALSE, output is generated with \code{\link[knitr]{asis_output}}; in this case, `results='asis'` is not necessary.
#'
#' @return A character string after interpolation.
#'
#' @author fnaufel
#'
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @importFrom knitr asis_output
#'
#' @export
#'
m <- function(
    ...,
    sep = ' ',
    sep_middle = '\n',
    before = NULL,
    sep_before = '\n',
    after = NULL,
    sep_after = '\n',
    sep_blocks = '\n',
    use_cat = getOption('m_use_cat', FALSE)
) {

  emit <- ifelse(use_cat, stringr::str_c, knitr::asis_output)

  if (!is.null(before)) {
    beg <- paste0(before, collapse = sep_before)
  } else {
    beg <- NA
  }

  if (!is.null(after)) {
    end <- paste0(after, collapse = sep_after)
  } else {
    end <- NA
  }

  middle <- glue::glue(
      ...,
      .open = '{{',
      .close = '}}',
      .sep = sep,
      .transformer = sympy_transformer
    )

  middle <- stringr::str_c(middle, collapse = sep_middle)

  final <- c(beg, middle, end)
  # Must delete NAs, else we get extra separators:
  final <- final[!is.na(final)]
  # beg, middle and end are length 1 vectors, so we get one string:
  final <- paste(final, collapse = sep_blocks)

  emit(final)

}


#' Custom transformer
#'
#' @param text Contents between one `{{...}}` pair.
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
    'comma',
    'period'
  )

  # Evaluate text in python
  obj <- reticulate::py_run_string(paste0('__m = ', text))
  obj <- obj[['__m']]

  # If list or vector, map latex function; result is a character vector
  if (is.vector(obj)) {
    m_latex <- purrr::map_chr(
      obj,
      ~reticulate::py$latex(., decimal_separator = decimal)
    )
  } else {
  # If single object, just call latex function
    m_latex <- reticulate::py$latex(obj, decimal_separator = decimal)
  }

  m_latex

}
